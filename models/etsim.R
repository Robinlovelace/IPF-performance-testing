############################################
#### IPFinR a script for IPF in R
#### Robin Lovelace (2013), after Malcolm Campell
############################################

# Initial conditions
start.time <- proc.time() # for measuring model runtime
l = 1
# for(l in 1:1){
k = l * 1
num.its = 3

# set working directory of input data
setwd("~/IPF-performance-testing/input-data/small-area-eg/") 
load("ind.RData")  # read-in the survey dataset called 'ind'

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

# setting totals to sum(con2) - replace con2 with most reliable constraint
con.pop <- rowSums(con2) 

con1 <- con1 * con.pop / rowSums(con1)
con2 <- con2 * con.pop / rowSums(con2) 
con3 <- con3 * con.pop / rowSums(con3)

sum(con1) == sum(con2) # check populations are equal

# setting-up reweighting data
category.labels <- names(all.msim) # should be correct from cons.R
all.mim.orig <- all.msim # save original (un-adjusted) constraints
all.msim <- cbind(con1 
                  ,con2
                  ,con3
                  #,con4 # add more if needed
                  )
all.msim[all.msim == 0] <- 0.00001 # avoid zeros

# aggregate values - column for each category
source("categorise.R") # this script must be customised to input data

# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 
sum(ind.cat[,ncol(con1)+ncol(con2)+1:ncol(con3)]) == nrow(ind) 

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,4][] <- 1 # sets initial weights to 1
weights[8,,4] <- k
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
wf <- array(dim=c(dim(weights), num.its))
indf <- array(dim=c(dim(ind.agg), num.its))
wf[,,,1] <- weights 
indf[,,,1] <- ind.agg

a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,4,1]))
t1 <- data.frame(it = 1, corr = cor(a.v,g.v))

for(it in 2:num.its){
source(file="e2.R")
wf[,,,it] <- weights
indf[,,,it] <- ind.agg
g.v <- as.vector(as.matrix(indf[,,4,it]))
t1[it,] <- c(it,cor(a.v,g.v))
}
barplot(height=t1$corr, names.arg=t1$it, ylim=c(t1[1,2],1))
t1
proc.time() - start.time 
