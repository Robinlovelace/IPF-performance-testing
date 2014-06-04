############################################
#### IPFinR a script for IPF in R 
############################################

# Initial conditions # start from IPF-performance-testing folder
num.its <- 3
# Read-in data (ensure working directory set correctly)
load("input-data/sheffield/ind.RData")  # read-in the survey dataset called 'ind'
# read aggregate constraints. nrow of these data frames (areas) must be equal 
source(file="models/sheffield/cons.R") # call separate (data specific) script to read in data, for modularity
num.cons <- length(grep(pattern="con[1-9]", x=ls()))  # calculate n. constraints (can set manually)
start.time <- proc.time()

# Checking that totals add up
sum(con1); sum(con2); sum(con3); sum(con4)

all.msim <- cbind(con1 
                  ,con2
                  ,con3
                  ,con4
                  )

con.pop <- rowSums(con2) 
con1 <- con1 * con.pop / rowSums(con1)
con2 <- con2 * con.pop / rowSums(con2) 
con3 <- con3 * con.pop / rowSums(con3)
con4 <- con4 * con.pop / rowSums(con4)

sum(con1); sum(con2); sum(con3); sum(con4)
con1[con1 == 0] <- con2[con2 == 0] <- con3[con3 == 0] <- con4[con4 == 0] <- 0.0001   
# save new outputs at this stage for the FMF model
# previous step avoids zero values (aren't any in this case...)

category.labels <- c ("m16_19","m20_24","m25_34","m35_54","m55_60","m60_plus","f16_19","f20_24","f25_34","f35_54","f55_60","f60_plus"
                      ,colnames(con2)
                      ,colnames(con3)
                      ,colnames(con4))
all.msim <- cbind(con1 
                  ,con2
                  ,con3
                  ,con4
                  )
# Aggregate values - column for each category
source("models/sheffield/categorise.R")
# Check constraint totals - should be true
sum(ind.cat[,1:12]) == nrow(ind) # 12 age/sex categories
sum(ind.cat[,13:23]) == nrow(ind) # 11 modes
sum(ind.cat[,24:31]) == nrow(ind) # 8 distance classes
sum(ind.cat[,32:40]) == nrow(ind) # 9 classes

# Create weights 
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,num.cons+1] <- 1 # sets initial weights to 1
ini.ws <- weights[,,num.cons+1]

# Convert survey data into aggregates to compare with census
ind.agg <- array(dim=c(nrow(all.msim),ncol(all.msim),num.cons+1))
for (i in 1:nrow(all.msim)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,num.cons+1]}

# re-weighting for constraint 1 via IPF 
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con1)){
    weights[which(ind.cat[,i] == 1),j,1] <- con1[j,i] /ind.agg[j,i,1]}}
for (i in 1:nrow(all.msim)){ # convert con1 weights back into aggregates
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1])}
# test the result
ind.agg[1:3,1:15,2]
all.msim[1:3,1:15]

# second constraint
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con2) + ncol(con1)){
    weights[which(ind.cat[,i] == 1),j,2] <- all.msim[j,i] /ind.agg[j,i,2]}}  
for (i in 1:nrow(all.msim)){ # convert con2 back into aggregate
  ind.agg[i,,3]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1] * weights[,i,2])}
ind.agg[1:3,1:15,3]
all.msim[1:3,1:15]
# third constraint
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con3) + ncol(con1) + ncol(con2)){
    weights[which(ind.cat[,i] == 1),j,3] <- all.msim[j,i] /ind.agg[j,i,3]}}
for (i in 1:nrow(all.msim)){ # convert con3 back into aggregate
  ind.agg[i,,4]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1] * weights[,i,2] * 
                                        weights[,i,3])}
# test the result
ind.agg[1:3,20:25,4]
all.msim[1:3,20:25]

# fourth constraint
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con4) + ncol(con1) + ncol(con2) + ncol(con3)){
    weights[which(ind.cat[,i] == 1),j,4] <- all.msim[j,i] /ind.agg[j,i,4]}}
for (i in 1:nrow(all.msim)){ # convert con3 back into aggregate
  ind.agg[i,,num.cons+1]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1] * 
                              weights[,i,2] * weights[,i,3] * weights[,i,4])}
ind.agg[1:3,31:32,5]
all.msim[1:3,31:32]

# for multiple iterations
wf <- array(dim=c(dim(weights), num.its, 1)) # array to store weights its, wei
indf <- array(dim=c(dim(ind.agg), num.its, 1))
wf[,,,1,1] <- weights 
indf[,,,1,1] <- ind.agg

# loop for multiple iterations (run e2.R repeatedly, saving each time)
for(it in 2:num.its){
  source(file="models/sheffield/e2.R")
  wf[,,,it,1] <- weights
  indf[,,,it,1] <- ind.agg
}

fw <-  weights[,,5] * weights[,,1] * weights[,,2] *  weights[,,3] * weights[,,4] # allocate final weight

### integerisations phase

intall <- ints <- as.list(1:nrow(all.msim)) # Names of integer indices (ints), and integer populations (intall) in ordered list
intagg <- all.msim * 0 # Aggregate area stats - set to 0 to avoid confusion
f <- floor(fw) # truncated weights
d <- fw - f

set.seed(0) # Include this line to ensure repeatable results

# Sample individuals based on their proportional probabilities
for (i in 1:nrow(all.msim)){
  if(max(f[,i]) == 0) f[which.max(fw[,i]),i] <- 1 # ensures model will run in case max(i5.w5 < 1) thanks to Eveline van Leeuwen
  ints[[i]] <- rep(which(fw[,i] > 0), f[,i])
  s <- sample(which(fw[,i] > 0), size = sum(con1[i,]) - sum(f[,i]) , # sample using decimal weights to 'top up' selection
              prob=d[,i], replace = F) 
  ints[[i]] <- c(ints[[i]], s) # add the sampled population to the one selected from truncated weights
  intall[[i]] <- ind[ints[[i]],] # Pulls all other data from index
  source("models/sheffield/areaCat.R") # save the aggregate data
  intagg[i,] <- colSums(area.cat) 
}

# intagg.trs <- intagg # save this result for comparison with other methods of integerisation

# to reformat the intall dataset, uncomment the following code:
intall.df <- cbind(intall[[1]], zone = 1)
head(intall.df)
for(i in 2:10){ # run for all zones with 1:length(intall)
  intall.df <- rbind(intall.df, cbind(intall[[i]], zone = i))
}
summary(intall.df[ intall.df$zone == 3, ]) # test the output
summary(intall.df[ intall.df$zone == 5, ]) # test the output

proc.time() - start.time  # analysis - see analyis files 
