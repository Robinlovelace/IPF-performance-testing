############################################
#### IPFinR a script for IPF in R
#### Robin Lovelace (2013), after Malcolm Campell
############################################

# Initial conditions # start from IPF-performance-testing folder
t1 <- data.frame(it=1,cor=0.995)
num.ws = 5 # Number of different weights to test
s.w = c(1:5) # the individuals with altered start weights
num.its = 2
# Read-in data (ensure working directory set correctly)
load("../../input-data/small-area-eg/ind.RData")  # read-in the survey dataset called 'ind'
# read aggregate constraints. nrow of these data frames (areas) must be equal 
source(file="cons.R") # call separate script to read in data, for modularity
# calculate number of constraints (objects with names con1, con2 etc):
num.cons <- length(grep(pattern="con[1-9]", x=ls())) 

# Checking that totals add up
sum(con1)
sum(con2)
sum(con3) # add more if need's be

all.msim <- cbind(con1
                  ,con2
                  ,con3
                  #,con4 # add more constraints here if needed
                  )

wf <- array(dim=c(dim(weights), num.its, num.ws))
indf <- array(dim=c(dim(ind.agg), num.its, num.ws))

# setting totals to sum(con2) - replace con2 with most reliable constraint
con.pop <- rowSums(con2) 

con1 <- con1 * con.pop / rowSums(con1)
con2 <- con2 * con.pop / rowSums(con2) 
con3 <- con3 * con.pop / rowSums(con3)

sum(con1) == sum(con2) # check populations are equal

con1[con1 == 0] <- 0.0001
con2[con2 == 0] <- 0.0001
con3[con3 == 0] <- 0.0001

# setting-up reweighting data
category.labels <- names(all.msim) # should be correct from cons.R
all.mim.orig <- all.msim # save original (un-adjusted) constraints
all.msim <- cbind(con1 
                  ,con2
                  ,con3
                  #,con4 # add more if needed
                  )

for(u in 1:num.ws){
  start.time <- proc.time() # for measuring model runtime
  k = u/5 + 0.5 # initial weight of sample individuals for testing

# aggregate values - column for each category
source("categorise.R") # this script must be customised to input data

# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 
sum(ind.cat[,ncol(con1)+ncol(con2)+1:ncol(con3)]) == nrow(ind) 

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,4][] <- 1 # sets initial weights to 1
weights[s.w,,4] <- k
ini.ws <- weights[,,4]

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim=c(nrow(all.msim),ncol(all.msim),num.cons+1))

for (i in 1:nrow(all.msim)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[i,1,4]}

# re-weighting for constraint 1 via IPF 
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con1)){
 weights[which(ind.cat[,i] == 1),j,1] <- con1[j,i] /ind.agg[j,i,1]}}

# convert con1 weights back into aggregates
for (i in 1:nrow(all.msim)){
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,4] * weights[,i,1])}

# test results for first row
ind.agg[1,1:12,2] - all.msim[1,1:12]

# second constraint
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con2) + ncol(con1)){
  weights[which(ind.cat[,i] == 1),j,2] <- all.msim[j,i] /ind.agg[j,i,2]}}  

# convert con2 back into aggregate
for (i in 1:nrow(all.msim)){
ind.agg[i,,3]   <- colSums(ind.cat * weights[,i,4] * weights[,i,1] *
                             weights[,i,2])}
# test results for first row
ind.agg[5,ncol(con1)+1:ncol(con2),3] 
all.msim[5,ncol(con1)+1:ncol(con2)]

# third constraint
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con3) + ncol(con1) + ncol(con2)){
    weights[which(ind.cat[,i] == 1),j,3] <- all.msim[j,i] /ind.agg[j,i,3]}}

# convert con3 back into aggregate
for (i in 1:nrow(all.msim)){
  ind.agg[i,,4]   <- colSums(ind.cat * weights[,i,4] * weights[,i,1] * weights[,i,2] * 
                               weights[,i,3])}
# for multiple iterations
wf[,,,1,u] <- weights 
indf[,,,1,u] <- ind.agg

a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,4,1,u]))

for(it in 2:num.its){
source(file="e2.R")
wf[,,,it,u] <- weights
indf[,,,it,u] <- ind.agg
g.v <- as.vector(as.matrix(indf[,,4,it,u]))
t1[it,1:2] <- c(it,cor(a.v,g.v))
t1$k <- k
}}
# Analysis (best to move this to analysis section)
barplot(height=t1[,2], names.arg=t1[,1], ylim=c(t1[1,2],1))
# plot(tw1[,1:2])
# points(tw1[,c(1,3)])
# points(tw1[,c(1,4)])
# points(tw1[,c(1,5)])
proc.time() - start.time 
