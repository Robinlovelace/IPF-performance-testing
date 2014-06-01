### code for generating input dataset with no empty cells
# run after empty cells.R to updated ind with no empty cells
load("input-data/small-area-eg/ind.RData")  # read-in the survey dataset called 'ind'
head(ind)

head(all.combs[Ind.missing,])
head(ind)
names(all.combs)[ind.miss[1,] == 1]

# allocate age and sex based on existing individuals
colC1 <- which(ind.miss[1,] == 1)[1]
ind.cat[which(ind.cat[colC1] == 1)[1], ] # test it's picking the right individuals
ind[which(ind.cat[colC1] == 1)[1], ]
fill.miss <- ind[which(ind.cat[colC1] == 1)[1], ]

# allocate marstat based on existing inds
colC2 <- which(ind.miss[1,] == 1)[2]
ind.cat[which(ind.cat[colC2] == 1)[1], ] # test it's picking the right individuals
ind[which(ind.cat[colC2] == 1)[1], ]
fill.miss$marstat <- ind$marstat[which(ind.cat[colC2] == 1)[1]]

# allocate house tenure based on existing ind.
colC3 <- which(ind.miss[1,] == 1)[3]
ind.cat[which(ind.cat[colC3] == 1)[1], ] # test it's picking the right individuals
ind[which(ind.cat[colC3] == 1)[1], ]
fill.miss$house <- ind$house[which(ind.cat[colC3] == 1)[1]]
ind <- rbind(ind, fill.miss)

# do it in a for loop
for(i in 1:nrow(ind.miss)){
  # allocate age and sex based on existing individuals
  colC1 <- which(ind.miss[i,] == 1)[1]
  ind.cat[which(ind.cat[colC1] == 1)[1], ] # test it's picking the right individuals
  ind[which(ind.cat[colC1] == 1)[1], ]
  fill.miss <- ind[which(ind.cat[colC1] == 1)[1], ]
  
  # allocate marstat based on existing inds
  colC2 <- which(ind.miss[i,] == 1)[2]
  ind.cat[which(ind.cat[colC2] == 1)[1], ] # test it's picking the right individuals
  ind[which(ind.cat[colC2] == 1)[1], ]
  fill.miss$marstat <- ind$marstat[which(ind.cat[colC2] == 1)[1]]
  
  # allocate house tenure based on existing ind.
  colC3 <- which(ind.miss[i,] == 1)[3]
  ind.cat[which(ind.cat[colC3] == 1)[1], ] # test it's picking the right individuals
  ind[which(ind.cat[colC3] == 1)[1], ]
  fill.miss$house <- ind$house[which(ind.cat[colC3] == 1)[1]]
  ind <- rbind(ind, fill.miss)
}