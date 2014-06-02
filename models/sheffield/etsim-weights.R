############################################
#### IPFinR a script for IPF in R
#### Robin Lovelace (2013)
#### run after etsim.R
############################################

# Initial conditions # start from IPF-performance-testing folder
num.ws <- 1 # Number of different weights to test
num.its <- 3
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,num.cons+1] <- 1 # sets initial weights to 1
ini.ws <- weights[,,num.cons+1]

set.seed(88)
s.w <- sample(1:nrow(ind),nrow(ind)/10) # the individuals with altered start weights

for(u in 1:num.ws){
  k <- 1000 # u # initial weight of sample individuals for testing
  weights[s.w,,4] <- k
# aggregate values - column for each category
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
}
proc.time() - start.time 
