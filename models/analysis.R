# analysis of all indicators

# z-score
zscore <- function(a.v, g.v) {pij <- a.v/sum(a.v)
                              rij <- g.v/sum(a.v)
                              zm <- (rij-pij)/sqrt((pij * (1 - pij))/sum(a.v))
                              sum(zm^2)}

# percent >5% deviation
perc.5 <- function(x, y){
  length(which(abs(x - y) > x * 0.05 ))/length(x)
}

a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,1,1,1])) # particular version you want

pcor <- cor(a.v, g.v, method="pearson")
TAE <- sum(abs(a.v - g.v))
SAE <- TAE/sum(a.v)
RMSE <- sqrt(sum((a.v -g.v)^2/length(a.v)))
Zs <- zscore(a.v, g.v) # see measures.R for function defining szcore
p5 <- perc.5(a.v, g.v) # see measures.R
iteration <- 1.1
measures <- data.frame(iteration, r, TAE, SAE, RMSE, Zs, E5)

for(i in 1:num.its){
  for(j in 1:num.cons){
    g.v <- as.vector(as.matrix(indf[,,j+1,i,1])) # particular version you want
    
    iteration <- paste(i,j,sep=".")
    r <- cor(a.v, g.v, method="pearson")
    TAE <- sum(abs(a.v - g.v))
    SAE <- TAE/sum(a.v)
    RMSE <- sqrt(sum((a.v -g.v)^2/length(a.v)))
    Zs <- zscore(a.v, g.v)
    E5 <- perc.5(a.v, g.v)
    
    measures[i*num.cons+j-num.cons,] <- c(iteration, r, TAE, SAE, RMSE, Zs, E5)
  }
}

class(measures[2:3,3]) # we've go character data

measures[] <- apply(measures, 2, as.numeric)

options(digits = 5)
measures
names(measures)
plot(measures[2:6,c(1,2,4,5)])
plot(measures[2:10,c(2,3,5,6)])

M <- cor(measures[,-1], use = "complete.obs")

# install.packages("corrplot")
library(corrplot)
corrplot.mixed(M)

# write the output 
# write.csv(measures, "models/sheffield/measures.csv")

