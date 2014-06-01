### IPFinR a script for IPF in R, Robin Lovelace (2013) - for "small area" example
# impact of empty cells

# create ind.cat with all cells
# nice maths in there, get it out!
all.combs <- matrix(NA, ncol = ncol(ind.cat), nrow = ncol(con1) * ncol(con2) )
C1 <- diag(ncol(con1)) # 'identity matrix' for constraint 1
C1 <- do.call("rbind", rep(list(C1), ncol(con2)))
C2 <- diag(ncol(con2))
C2 <- C2[rep(1:nrow(C2), each = ncol(con1)), ]
C2 <- do.call("rbind", rep(list(C2)))
all.combs <- cbind(C1, C2)
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
all.combs[Ind.missing,]

set.seed(999)
addMiss <- sample(Ind.unique,1)
ind.miss1 <- ind[-which(p.ind.cat %in% p.all.combs[addMiss]),]
nrow(ind)
nrow(ind.miss1)

# factor analysis
p.ind.cat <- as.factor(p.ind.cat)
summary(p.ind.cat) # the most common types of individuals across all categories
