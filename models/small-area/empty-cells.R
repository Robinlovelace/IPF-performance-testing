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
all.combs <- data.frame(all.combs)
names(all.combs) <- names(ind.cat)

head(all.combs)

nrow(unique(all.combs))
nrow(unique(ind.cat))

n.empty <- nrow(unique(all.combs)) -
nrow(unique(ind.cat))

n.empty == 0 # is ind complete? (FALSE means it has empty cells)

p.all.combs <- apply(all.combs, 1, paste, collapse = "")
p.ind.cat <- apply(ind.cat, 1, paste, collapse = "")
head(p.all.combs)

Ind.unique <- which(p.all.combs %in% p.ind.cat)
Ind.missing <- which(!p.all.combs %in% p.ind.cat)
ind.miss <- all.combs[Ind.missing,]

set.seed(999)
addMiss <- sample(Ind.unique, 
                  1
#                   length(Ind.unique)/10 )
                  )

ind.miss1 <- ind[-which(p.ind.cat %in% p.all.combs[addMiss]),]
ind.miss10p <- ind[-which(p.ind.cat %in% p.all.combs[addMiss]),]
nrow(ind)
nrow(ind.miss1)
nrow(ind.miss10p)
# save the individual-level data with more empty cells 
# (uncomment this for 1 more empty cell line below for 10% more)
# ind <- ind.miss1 
# ind <- ind.miss10p

# # factor analysis
# p.ind.cat <- as.factor(p.ind.cat)
# summary(p.ind.cat) # the most common types of individuals across all categories
# ind.cat.m1 <- ind.cat[-sample(Ind.unique,1)

