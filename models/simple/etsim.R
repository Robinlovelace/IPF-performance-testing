############################################
#### IPFinR a script for IPF in R 
#### Robin Lovelace (2013)
############################################

num.its <- 3 # how many iterations will we run?

# Loading the data: Ensure R is in the right working directory 
# (input-data/simple or models/simple). Check this by entering getwd()
# new working directories can be set with setwd("directory") 
# alternatively, RStudio can set the working directory.
# E.g. Session > Set working directory > To Source file location
# The R will be able to 'see' the files to be loaded 
list.files() # you should see age.csv and other input data here

ind <- read.csv("ind.csv")
con1 <- read.csv("age.csv") # add the age constraint data 
con2 <- read.csv("sex.csv") # add the sex constraint data
num.cons <- length(grep(pattern="con[1-9]", x=ls()))  # calculate n. constraints (can set manually)

# checking that totals add up, add more if need's be
sum(con1)
sum(con2) # check populations are equal
all.msim <- cbind(con1, con2)

# setting-up reweighting data
category.labels <- names(all.msim) # should be correct from cons.R

# set-up aggregate values - column for each category
source("categorise.R") # this script must be customised to input data
ind.cat[1:3, 1:4] # take a look at the first 2 rows and 4 columns of ind.cat - as expected?
# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct?
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,num.cons+1][] <- 1 # sets initial weights to 1
ini.ws <- weights[,,num.cons+1]

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim=c(nrow(all.msim),ncol(all.msim),num.cons+1))
for (i in 1:nrow(all.msim)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,num.cons+1]}
ind.agg # look at what we've created - n. individuals replicated throughout

############## The IPF part #############

# Re-weighting for constraint 1 via IPF 
for (j in 1:nrow(all.msim)){
  for(i in 1:ncol(con1)){
 weights[which(ind.cat[,i] == 1),j,1] <- con1[j,i] /ind.agg[j,i,1]}}
for (i in 1:nrow(all.msim)){ # convert con1 weights back into aggregates
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1])}
# test results for first row (not necessary for model)
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

############## The multiple iterations #############

# loop for multiple iterations (run e2.R repeatedly, saving each time)
for(it in 2:num.its){
source(file="e2.R")
wf[,,,it,1] <- weights
indf[,,,it,1] <- ind.agg
}

############## The analysis part #############

a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,1,2,1]))
cor(a.v,g.v)

t1 <- data.frame(it = 1, corr = cor(a.v,g.v))
t1 <- t1[0,]
for(it in 1:num.its){
  for(con in 2:(num.cons+1)){
    g.v <- as.vector(as.matrix(indf[,,con,it,1]))
    t1[nrow(t1)+1,] <- c(it+con/10,cor(a.v,g.v))
  }
}
t1
t1$numit<-1:nrow(t1)

############## Plot the results #############

# plot the increasing fit, one interation to the next 
barplot(height=t1[,2], names.arg=t1[,1], ylim=c(t1[1,2],1), ylab=("Correlation (r)"))