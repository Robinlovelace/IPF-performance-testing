### IPFinR a script for IPF in R, Robin Lovelace (2013) - for "small area" example
# impact of empty cells

# read-in data (ensure working directory set to file location)
load("input-data/small-area-eg/ind.RData")  # read-in the survey dataset called 'ind'
head(ind)

source(file = "models/small-area/cons.R") 
all.msim <- cbind(con1, con2, con3) 
category.labels <- names(all.msim)  # should be correct from cons.R

source("models/small-area/categorise.R") 
head(ind.cat)

# create ind.cat with all cells
# nice maths in there, get it out!
all.combs <- matrix(NA, ncol = ncol(ind.cat), nrow = ncol(con1) * ncol(con2) * ncol(con3) )
C1 <- diag(ncol(con1)) # 'identity matrix' for constraint 1
C1 <- do.call("rbind", rep(list(C1), ncol(con2) * ncol(con3)))
C2 <- diag(ncol(con2))
C2 <- C2[rep(1:nrow(C2), each = ncol(con1)), ]
C2 <- do.call("rbind", rep(list(C2), ncol(con3)))
C3 <- diag(ncol(con3))
C3 <- C3[rep(1:nrow(C3), each = ncol(con1) * ncol(con2)), ]
all.combs <- cbind(C1, C2, C3)
nrow(unique(all.combs))
