# analysis.R - analysis of baseline scenario

# evaluation using correlation indices
a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,1,1,1]))
cor(a.v,g.v)

t1 <- data.frame(it = 1, corr = cor(a.v,g.v))
t1 <- t1[0,]
for(it in 1:num.its){
  for(con in 2:(num.cons+1)){
      g.v <- as.vector(as.matrix(indf[,,con,it,1]))
  t1[nrow(t1)+1,] <- c(it+con/10,cor(a.v,g.v))
  }
}
t1
t1$numit<-1:nrow(t1)
barplot(height=t1[,2], names.arg=t1[,1], ylim=c(t1[1,2],1), ylab=("Correlation (r)"))


