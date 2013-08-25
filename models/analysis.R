# analysis
a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,1,1,1])) # particular version you want

pcor <- cor(a.v, g.v, method="pearson")
tae <- sum(abs(a.v - g.v))
sae <- tae/sum(a.v)
rmse <- sqrt(sum((a.v -g.v)^2/length(a.v)))
zs <- zscore(a.v, g.v)
p5 <- perc.5(a.v, g.v)
iteration <- 1.1
measures <- data.frame(pcor, tae, sae, rmse, zs, p5)

for(i in 1:num.its){
  for(j in 1:num.cons){
    g.v <- as.vector(as.matrix(indf[,,j+1,i,1])) # particular version you want
    
    iteration[i*2+j-2] <- paste(i,j,sep=".")
    pcor <- cor(a.v, g.v, method="pearson")
    tae <- sum(abs(a.v - g.v))
    sae <- tae/sum(a.v)
    rmse <- sqrt(sum((a.v -g.v)^2/length(a.v)))
    zs <- zscore(a.v, g.v)
    p5 <- perc.5(a.v, g.v)
    
    measures[i*2+j-2,] <- c(pcor, tae, sae, rmse, zs, p5)
  }
}
options(digits = 5)
measures
plot(measures[2:6,c(1,2,4,5)])
measures <- cbind(iteration, measures)
measures
write.csv(measures, "measures.csv")

