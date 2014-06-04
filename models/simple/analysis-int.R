# analysis.R - basic analysis file for analysing model output
# z-score
zscore <- function(a.v, g.v) {pij <- a.v/sum(a.v)
                              rij <- g.v/sum(a.v)
                              zm <- (rij-pij)/sqrt((pij * (1 - pij))/sum(a.v))
                              sum(zm^2)}
# percent >5% deviation
perc.5 <- function(x, y){
  length(which(abs(x - y) > x * 0.05 ))/length(x)
}


measures <- data.frame(pcor, tae, sae, rmse, zs, p5)
measures
a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(intagg)) # particular version you want

pcor <- cor(a.v, g.v, method="pearson")
tae <- sum(abs(a.v - g.v))
sae <- tae/sum(a.v)
rmse <- sqrt(sum((a.v -g.v)^2/length(a.v)))
zs <- zscore(a.v, g.v) # see measures.R for function defining szcore
p5 <- perc.5(a.v, g.v) # see measures.R
iteration <- 1.1
measures <- data.frame(pcor, tae, sae, rmse, zs, p5)

measures




