############################################
#### IPFinR a script for IPF in R 
############################################
library(dplyr)
library(ipfp)

# Initial conditions # start from IPF-performance-testing folder
num.its <- 10
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

ind.cat.orig <- ind.cat # save original
ind.cat.p <- do.call(paste0, c(ind.cat))
ind.cat.p <- data.frame(u = ind.cat.p)
ind.cat <- ind.cat[!duplicated(ind.cat.p),]
ind.cat.p2 <- do.call(paste0, c(ind.cat))
ind.cat.p2 <- data.frame(u = ind.cat.p2)
ind.cat.p[1:10,]
ind.cat.p2[1:10,]

ind.t <- data.frame(table(ind.cat.p))
names(ind.t)[1] <- "u"
ind.t <- merge(ind.cat.p2, ind.t, by = "u", sort = F)
head(ind.t)
head(ind.cat.p2)

# Create weights 
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,num.cons+1] <- 1 # sets initial weights to 1
ini.ws <- weights[,,num.cons+1]

# Convert survey data into aggregates to compare with census
ind.agg <- array(dim=c(nrow(all.msim),ncol(all.msim),num.cons+1))
for (i in 1:nrow(all.msim)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,num.cons+1]}

A <- t(ind.cat) # the constraint matrix for ipfp
x0 <- rep(1, nrow(ind.cat)) / ind.t$Freq

# create weights in 3D matrix (individuals, areas, iteration)
# weights_ipf <- array(data = 1, dim=c(nrow(ind.cat),nrow(all.msim)) )

ind_agg_ipf <- all.msim

for(i in 1:nrow(all.msim)){
  y <- as.numeric(all.msim[i,]) # the constraint vector to be emulated
  weights_ipf <- ipfp(y, A, x0, verbose = F, maxit = num.its)
  # analysis of the weights to extract true weight
#   summary(weights_ipf)
#   sum(weights_ipf) # correct total weight
  ind.t$w <- weights_ipf / ind.t$Freq 
  w_final <- inner_join(ind.cat.p, ind.t)$w
#   summary(w_final)
#   sum(w_final)
  ind_agg_ipf[i, ] <- colSums(w_final * ind.cat.orig)  
}

all.msim[1:2,1:5]
ind_agg_ipf[1:2,1:5]
cor(as.vector(as.matrix(ind_agg_ipf[2,])), as.vector(as.matrix(all.msim[1,])))
# cor(as.vector(as.matrix(all.msim)), as.vector(as.matrix(indf[,,4,it,1])))
# cor(as.vector(ind_agg_ipf), as.vector(as.matrix(indf[,,4,it,1])))


proc.time() - start.time # Analysis - see analyis files