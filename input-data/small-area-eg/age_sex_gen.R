# script to build age/sex constraint

# create ages
alabs <- c("11-20", "21-30", "31-50", "50+")
p <- c(0.15, 0.15, 0.2, 0.5) # probability of different ages

# create counts
m <- rowSums(con1[grep("m", names(con1))])
f <- rowSums(con1[grep("f", names(con1))])

set.seed(1066)
mcon4 <- matrix(ncol = length(alabs), nrow = nrow(con1))
for(i in 1:nrow(con1)){
  s <- sample(1:4, size = m[i] + 1, replace = T, prob = p)
  mcon4[i, ] <- summary(factor(s))
}

fcon4 <- mcon4 <- matrix(ncol = length(alabs), nrow = nrow(con1))
for(i in 1:nrow(con1)){
  s <- sample(1:4, size = f[i] + 1, replace = T, prob = p)
  fcon4[i, ] <- summary(factor(s, levels = 1:4))
}

for(i in 1:nrow(con1)){
  s <- sample(1:4, size = m[i] + 1, replace = T, prob = p)
  mcon4[i, ] <- summary(factor(s, levels = 1:4))
}

con4 <- cbind(mcon4, fcon4)
con4 <- as.data.frame(con4)
mn <- paste0("m", alabs)
names(con4) <- c(paste0("m", alabs), paste0("f", alabs))
names(con4) <- gsub("-", "_", names(con4))
head(con4)

rm(fcon4, mcon4, f, m)
