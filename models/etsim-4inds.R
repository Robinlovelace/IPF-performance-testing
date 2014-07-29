############################################
#### IPFinR a script for IPF in R 
#### Robin Lovelace (2013)
############################################

# Initial conditions # start from IPF-performance-testing folder
num.its <- 3
# Read-in data (manually to start, will use scripts in future)
c.names <- c("id", "age", "sex")
ind <- c(       1, 59, "m",
                3, 35, "m", 
                4, 73, "f", 
                5, 49, "f")
ind <- matrix(ind, nrow = 4, byrow = T) # Convert long data into matrix, by row
ind <- data.frame(ind) # Convert this into a dataframe
names(ind) <- c.names # Add correct column names
ind$age <- as.numeric(levels(ind$age)[ind$age]) # Age is a numeric variable

ind # Show the data frame in R

# Read in the data in constraints
category.labels <- c("16-49", "50+",  "m", "f") # Age, and sex constraints
all.msim <- c(        8,        4,     6,    6,  
                      2,        8,     4,    6,  
                      7,        4,     3,    8,   
                      5,        4,     7,    2,   
                      7,        3,     6,    4,   
                      5,        3,     2,    6    
) # each row represents an area, each column a category
all.msim <- matrix(all.msim, ncol = 4, byrow = T) # Convert long data into matrix, by row
all.msim <- data.frame(all.msim) # Convert this into a dataframe
names(all.msim) <- category.labels # Add labels
con1 <- all.msim[,1:2] ; con2 <- all.msim[,3:4]

num.cons <- length(grep(pattern="con[1-9]", x=ls()))  # calculate n. constraints (can set manually)

# Checking that totals add up
sum(con1)
sum(con2)
# sum(con3) # add more if need's be

all.msim <- cbind(con1 # usually constraints added separately; added here for illustration
                  ,con2
                  #,con3
                  #,con4 # add more constraints here if needed
                  )

# setting totals to sum(con2) - as totals do not add up (unnecessary here as they do...)
con.pop <- rowSums(con2) 
con1 <- con1 * con.pop / rowSums(con1)
con2 <- con2 * con.pop / rowSums(con2)
# con3 <- con3 * con.pop / rowSums(con3)
con.pop
all.msim

sum(con1) == sum(con2) # check populations are equal
# zero vallues to 0.0001
con1[con1 == 0] <- 0.0001; con2[con2 == 0] <- 0.0001 #; con3[con3 == 0] <- 0.0001   

# setting-up reweighting data
category.labels <- names(all.msim) # should be correct from cons.R
all.mim.orig <- all.msim # save original (un-adjusted) constraints
all.msim <- cbind(con1 
                  ,con2
                  #,con3
                  #,con4 # add more if needed
                  )

start.time <- proc.time() # for measuring model runtime
# aggregate values - column for each category
source("models/simple/categorise.R") # this script must be customised to input data
# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 
# sum(ind.cat[,ncol(con1)+ncol(con2)+1:ncol(con3)]) == nrow(ind)  # no 3rd constraint, commented out

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,num.cons+1][] <- 1 # sets initial weights to 1
ini.ws <- weights[,,num.cons+1]

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim=c(nrow(all.msim),ncol(all.msim),num.cons+1))
for (i in 1:nrow(all.msim)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,num.cons+1]}
ind.agg # look at what we've created - n. individuals replicated throughout

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

# # third constraint - not needed in this case - commented out
# for (j in 1:nrow(all.msim)){
#   for(i in 1:ncol(con3) + ncol(con1) + ncol(con2)){
#     weights[which(ind.cat[,i] == 1),j,3] <- all.msim[j,i] /ind.agg[j,i,3]}}
# for (i in 1:nrow(all.msim)){ # convert con3 back into aggregate
#   ind.agg[i,,4]   <- colSums(ind.cat * weights[,i,4] * weights[,i,1] * weights[,i,2] * 
#                                weights[,i,3])}
# for multiple iterations
wf <- array(dim=c(dim(weights), num.its, 1)) # array to store weights its, wei
indf <- array(dim=c(dim(ind.agg), num.its, 1))
wf[,,,1,1] <- weights 
indf[,,,1,1] <- ind.agg

# loop for multiple iterations (run e2.R repeatedly, saving each time)
for(it in 2:num.its){
source(file="models/simple/e2.R")
wf[,,,it,1] <- weights
indf[,,,it,1] <- ind.agg
}
proc.time() - start.time 

# Analysis - in general, see analyis files
a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,1,1,1]))
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
# plot the increasing fit, one interation to the next 
barplot(height=t1[,2], names.arg=t1[,1], ylim=c(t1[1,2],1), ylab=("Correlation (r)"))