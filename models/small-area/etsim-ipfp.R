### IPFinR a script for IPF in R, Robin Lovelace (2013) - for "small area" example

# initial conditions - start from IPF-performance-testing folder
num.its <- 3
# read-in data (ensure working directory set to file location)
load("input-data/small-area-eg/ind.RData")  # read-in the survey dataset called 'ind'
# read aggregate constraints. nrow of these data frames (areas) must be equal
source(file = "models/small-area/cons.R")  # call separate (data specific) script to read in data, for modularity
num.cons <- length(grep(pattern = "con[1-9]", x = ls()))  # calculate n. constraints (can set manually)

sum(con1); sum(con2); sum(con3) # checking that totals add up
# all.msim is the all the constraint variables combined in one data frame
all.msim <- cbind(con1, con2, con3)  # add more constraints if needed

# setting totals to sum(con2) - as totals do not add up
con.pop <- rowSums(con2)
con1 <- con1 * con.pop/rowSums(con1)
con2 <- con2 * con.pop/rowSums(con2)
con3 <- con3 * con.pop/rowSums(con3)
sum(con1) == sum(con2)  # check populations are equal
con1[con1 == 0] <- con2[con2 == 0] <- con3[con3 == 0] <- 1e-04 # zero values to 0.0001

# setting-up reweighting data
category.labels <- names(all.msim)  # should be correct from cons.R
all.mim.orig <- all.msim  # save original (un-adjusted) constraints
all.msim <- cbind(con1, con2, con3)
start.time <- proc.time()  # for measuring model runtime

# aggregate values - column for each category
source("models/small-area/categorise.R")  # this script must be customised to input data
# check constraint totals - should be true

A <- t(ind.cat) # the constraint matrix for ipfp
x0 <- rep(1, nrow(ind))

# create weights in 3D matrix (individuals, areas, iteration)
weights_ipf <- array(data = 1, dim=c(nrow(ind),nrow(all.msim)) )

for(i in 1:ncol(weights_ipf)){
  y <- as.numeric(all.msim[i,]) # the constraint vector to be emulated
  weights_ipf[,i] <- ipfp(y, A, x0, verbose = F, maxit = num.its)
}

ind_agg_ipf <- t(apply(weights_ipf, MARGIN = 2, FUN = function(x) colSums(x * ind.cat)))
all.msim[1,]
ind_agg_ipf[1,]

proc.time() - start.time  # analysis - see analyis files 
