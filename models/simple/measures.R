# measures - metrics used to evaluate model fit

# preliminaries: split data frame into vectors to compare with all.msim
a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,3,1,1])) # particular version you want
# you can use subsets of the data frame, e.g. certain constraints (columns)

# correlation
pcor <- cor(a.v, g.v, method="pearson")
pcor

# tae - total absolute error
abs(a.v - g.v)
tae <- sum(abs(a.v - g.v))

# sae - standardised absolute error
sae <- tae/sum(a.v)
sae

# rmse - root mean square error
rmse <- sqrt(sum((a.v -g.v)^2/length(a.v)))

# z-score
zscore <- function(a.v, g.v) {pij <- a.v/sum(a.v)
rij <- g.v/sum(a.v)
zm <- (rij-pij)/sqrt((pij * (1 - pij))/sum(a.v))
sum(zm^2)}
zs <- zscore(a.v, g.v)

# percent >5% deviation
perc.5 <- function(x, y){
  length(which(abs(x - y) > x * 0.05 ))/length(x)
}
p5 <- perc.5(a.v, g.v)
measures <- data.frame(pcor, tae, sae, rmse, zs, p5)
measures
