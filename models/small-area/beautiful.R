### IPFinR a script for IPF in R Robin Lovelace (2013)

# initial conditions - start from IPF-performance-testing folder
num.its = 3
# read-in data (ensure working directory set correctly)
load("../../input-data/small-area-eg/ind.RData")  # read-in the survey dataset called 'ind'
# read aggregate constraints. nrow of these data frames (areas) must be equal
source(file = "cons.R")  # call separate (data specific) script to read in data, for modularity
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
source("categorise.R")  # this script must be customised to input data
# check constraint totals - should be true
sum(ind.cat[, 1:ncol(con1)]) == nrow(ind)  # is the number in each category correct
sum(ind.cat[, ncol(con1) + 1:ncol(con2)]) == nrow(ind)
sum(ind.cat[, ncol(con1) + ncol(con2) + 1:ncol(con3)]) == nrow(ind)

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim = c(nrow(ind), nrow(all.msim), num.cons + 1))
weights[, , num.cons + 1][] <- 1  # sets initial weights to 1
ini.ws <- weights[, , num.cons + 1]

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim = c(nrow(all.msim), ncol(all.msim), num.cons + 1))
for (i in 1:nrow(all.msim)) {
    ind.agg[i, , 1] <- colSums(ind.cat) * weights[1, i, num.cons + 1]
}
# re-weighting for constraint 1 via IPF
for (j in 1:nrow(all.msim)) {
    for (i in 1:ncol(con1)) {
        weights[which(ind.cat[, i] == 1), j, 1] <- con1[j, i]/ind.agg[j, i, 1]
    }
}
for (i in 1:nrow(all.msim)) {
    # convert con1 weights back into aggregates
    ind.agg[i, , 2] <- colSums(ind.cat * weights[, i, num.cons + 1] * weights[, 
        i, 1])
}
# second constraint
for (j in 1:nrow(all.msim)) {
    for (i in 1:ncol(con2) + ncol(con1)) {
        weights[which(ind.cat[, i] == 1), j, 2] <- all.msim[j, i]/ind.agg[j, i, 
            2]
    }
}
for (i in 1:nrow(all.msim)) {
    ind.agg[i, , 3] <- colSums(ind.cat * weights[, i, num.cons + 1] * weights[, 
        i, 1] * weights[, i, 2])
}
# third constraint
for (j in 1:nrow(all.msim)) {
    for (i in 1:ncol(con3) + ncol(con1) + ncol(con2)) {
        weights[which(ind.cat[, i] == 1), j, 3] <- all.msim[j, i]/ind.agg[j, i, 
            3]
    }
}
for (i in 1:nrow(all.msim)) {
    ind.agg[i, , num.cons + 1] <- colSums(ind.cat * weights[, i, num.cons + 1] * 
        weights[, i, 1] * weights[, i, 2] * weights[, i, 3])
}
# for multiple iterations
wf <- array(dim = c(dim(weights), num.its, 1))  # array to store weights its, wei
indf <- array(dim = c(dim(ind.agg), num.its, 1))
wf[, , , 1, 1] <- weights
indf[, , , 1, 1] <- ind.agg
# loop for multiple iterations (run e2.R repeatedly, saving each time)
for (it in 2:num.its) {
    source(file = "e2.R")
    wf[, , , it, 1] <- weights
    indf[, , , it, 1] <- ind.agg
}
proc.time() - start.time  # Analysis - see analyis files 