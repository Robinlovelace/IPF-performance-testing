# Minimal IPF example
start_time <- Sys.time()
num.its <- 10 # iterations

# Read-in data (manually to start, will use scripts in future)
c.names <- c("id", "age", "sex")
ind <- c(       1, "50+", "m", 
                3, "16-49", "m", # removed superfluous individual
                4, "50+", "f", 
                5, "16-49", "f")
ind <- matrix(ind, nrow = 4, byrow = T) # Convert long data into matrix, by row
ind <- data.frame(ind) # Convert this into a dataframe
names(ind) <- c.names # Add correct column names

# Read in the data in constraints
category.labels <- c("16-49", "50+",  "m", "f") # Age, and sex constraints
all.msim <- c(        8,        4,     6,    6  
) # each row represents an area, each column a category
all.msim <- matrix(all.msim, ncol = 4, byrow = T) # Convert long data into matrix, by row
all.msim <- data.frame(all.msim) # Convert this into a dataframe
names(all.msim) <- category.labels # Add labels
con1 <- all.msim[,1:2] ; con2 <- all.msim[,3:4]

ind.cat <- data.frame(cbind(model.matrix(~ind$sex - 1)[,c(2,1)], model.matrix(~ind$age - 1)))
names(ind.cat) <- category.labels

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,num.cons+1][] <- 1 # sets initial weights to 1
ini.ws <- weights[,,num.cons+1]

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim=c(nrow(all.msim),ncol(all.msim),num.cons+1))
for (i in 1:nrow(all.msim)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,num.cons+1]}

# re-weighting for constraint 1 via IPF 
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con1)){
 weights[which(ind.cat[,i] == 1),j,1] <- con1[j,i] /ind.agg[j,i,1]}}
for (i in 1:nrow(all.msim)){ # convert con1 weights back into aggregates
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1])}

# test results for first row
ind.agg[1,1:2,2] - all.msim[1,1:2] # should be zero

# second constraint
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con2) + ncol(con1)){
  weights[which(ind.cat[,i] == 1),j,2] <- all.msim[j,i] /ind.agg[j,i,2]}}  
for (i in 1:nrow(all.msim)){ # convert con2 back into aggregate
ind.agg[i,,3]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1] * weights[,i,2])}

# for multiple iterations
wf <- array(dim=c(dim(weights), num.its, 1)) # array to store weights its, wei
indf <- array(dim=c(dim(ind.agg), num.its, 1))
wf[,,,1,1] <- weights 
indf[,,,1,1] <- ind.agg

# loop for multiple iterations (run e2.R repeatedly, saving each time)
for(it in 2:num.its){
source(file="e2.R")
wf[,,,it,1] <- weights
indf[,,,it,1] <- ind.agg
}

# Analysis - in general, see analyis files
a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,1,1,1]))
cor(a.v,g.v)
(total_time <- Sys.time() - start_time)
