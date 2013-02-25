############################################
#### IPFinR a script for IPF in R
#### Robin Lovelace (2013), after Malcolm Campell
############################################

# loading data
start.time <- proc.time() # for measuring model runtime

# set working directory of input data
setwd("~/IPF-performance-testing/input-data/small-area-eg/") 

load("ind.RData")  # read-in the survey dataset. Preprocessed to 
# to contain the desired microdataset and saved in a dataframe called 'ind'

# read aggregate constraints. nrow of these data frames (areas) must be equal 
source(file="cons.R") # call separate script to read in data, for modularity
# calculate number of constraints (objects with names con1, con2 etc):
num.cons <- length(grep(pattern="con[1-9]", x=ls())) 

# Checking that totals add up
sum(con1)
sum(con2)
sum(con3) # add more if need's be

all.msim <- cbind(con1 
                  # comment out constraints not included
                  ,con2
                  ,con3
                  #,con4 # add more constraints here if needed
                  )

# setting totals to sum(con2) - replace con2 with most reliable constraint
con.pop <- rowSums(con2) 

con1 <- con1 * con.pop / rowSums(con1)
con2 <- con2 * con.pop / rowSums(con2) 
con3 <- con3 * con.pop / rowSums(con3)

sum(con1) == sum(con2) 
sum(con2) == sum(con3)

# IF CELL VALUE == 0, SET to 0.0001 (a very small number)
con1[con1 == 0]   <- 0.0001 
con2[con2 == 0]   <- 0.0001 
con3[con3 == 0]   <- 0.0001 # add more constraints if needed

# setting-up reweighting data
category.labels <- names(all.msim) # should be correct from cons.R
all.msim <- cbind(con1 
                  ,con2
                  ,con3
                  #,con4 # add more if needed
                  )

# aggregate values - column for each category
source("categorise.R") # this script must be customised to input data

# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 
sum(ind.cat[,ncol(con1)+ncol(con2)+1:ncol(con3)]) == nrow(ind) 

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,1] <- 1 # sets initial weights to 1

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim=c(nrow(all.msim),ncol(all.msim),num.cons+1))

for (i in 1:nrow(all.msim)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[i,1,1]}

# re-weighting for constraint 1 via IPF 
for (j in 1:nrow(all.msim)){
 weights[which(ind.cat[,1] == 1),j,1] <- con1[j,1] /ind.agg[j,1,1]
 weights[which(ind.cat[,2] == 1),j,1] <- con1[j,2] /ind.agg[j,2,1]
 weights[which(ind.cat[,3] == 1),j,1] <- con1[j,3] /ind.agg[j,3,1]
 weights[which(ind.cat[,4] == 1),j,1] <- con1[j,4] /ind.agg[j,4,1]
 weights[which(ind.cat[,5] == 1),j,1] <- con1[j,5] /ind.agg[j,5,1]
 weights[which(ind.cat[,6] == 1),j,1] <- con1[j,6] /ind.agg[j,6,1]
 weights[which(ind.cat[,7] == 1),j,1] <- con1[j,7] /ind.agg[j,7,1]
 weights[which(ind.cat[,8] == 1),j,1] <- con1[j,8] /ind.agg[j,8,1]
 weights[which(ind.cat[,9] == 1),j,1] <- con1[j,9] /ind.agg[j,9,1]
 weights[which(ind.cat[,10] == 1),j,1] <- con1[j,10] /ind.agg[j,10,1]
 weights[which(ind.cat[,11] == 1),j,1] <- con1[j,11] /ind.agg[j,11,1]
 weights[which(ind.cat[,12] == 1),j,1] <- con1[j,12] /ind.agg[j,12,1]}

# convert con1 weights back into aggregates
for (i in 1:nrow(all.msim)){
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,1])}

# test results for first row
ind.agg[1,1:12,2] - all.msim[1,1:12]

# second constraint
for (j in 1:nrow(all.msim)){
  weights[which(ind.cat[,13] == 1),j,2] <- all.msim[j,13] /ind.agg[j,13,2]
  weights[which(ind.cat[,14] == 1),j,2] <- all.msim[j,14] /ind.agg[j,14,2]
  weights[which(ind.cat[,15] == 1),j,2] <- all.msim[j,15] /ind.agg[j,15,2]
  weights[which(ind.cat[,16] == 1),j,2] <- all.msim[j,16] /ind.agg[j,16,2]
  weights[which(ind.cat[,17] == 1),j,2] <- all.msim[j,17] /ind.agg[j,17,2]}  

# convert con2 back into aggregate
for (i in 1:nrow(all.msim)){
ind.agg[i,,3]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2])}

# test results for first row
ind.agg[5,ncol(con1)+1:ncol(con2),3] 
all.msim[5,ncol(con1)+1:ncol(con2)]

# third constraint
for (j in 1:nrow(all.msim)){
  weights[which(ind.cat[,18] == 1),j,3] <- all.msim[j,18] /ind.agg[j,18,3]
  weights[which(ind.cat[,19] == 1),j,3] <- all.msim[j,19] /ind.agg[j,19,3]
  weights[which(ind.cat[,20] == 1),j,3] <- all.msim[j,20] /ind.agg[j,20,3]
  weights[which(ind.cat[,21] == 1),j,3] <- all.msim[j,21] /ind.agg[j,21,3]
  weights[which(ind.cat[,22] == 1),j,3] <- all.msim[j,22] /ind.agg[j,22,3]}  

# convert con3 back into aggregate
for (i in 1:nrow(all.msim)){
  ind.agg[i,,4]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2] * 
                               weights[,i,3])}
# test results for first row
ind.agg[5,ncol(con1)+ncol(con2)+1:ncol(con3),4] 
all.msim[5,ncol(con1)+ncol(con2)+1:ncol(con3)]

# for multiple iterations
num.its <- 5 # change number of iterations 
weights[,,num.cons+1] <- weights[,,1] * weights[,,2] *  weights[,,3])
aagg <- array(dim=c(dim(ind.agg),num.its))
aagg[,,,1] <- ind.agg
dim(aagg)

# Test results for first row
time1 <- proc.time() -start.time
time1

### Basic analysis
a.v <- as.vector(as.matrix(all.msim))
g.v <- as.vector(as.matrix(ind.agg[,,num.cons+1]))
plot(a.v, g.v)

#### Iterations: uncomment following lines to run 10 iterations of IPF

# setwd("its/")
# source(file="etsim2.r") # Include to run second iteration
# source(file="etsim3.r") #
# source(file="etsim4.r") #
# source(file="etsim5.r") #
# source(file="etsim6.r") #
# source(file="etsim7.r") #
# source(file="etsim8.r") #
# source(file="etsim9.r") #
# source(file="etsim10.r") #
proc.time() - start.time

