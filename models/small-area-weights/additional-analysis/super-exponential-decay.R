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
# plot(tw1[,1:2])
# points(tw1[,c(1,3)])
# points(tw1[,c(1,4)])
# points(tw1[,c(1,5)])
plot(5:12,t1[5:12,2])
t1 <- t1[4:18,]
t1$numit <- 1:15
t1lm <- lm(t1$corr ~log(t1$numit))
summary(t1lm)
p1 <- predict(t1lm, data.frame(4:18))
plot(t1$numit, t1$corr)
t1gm <- glm(t1$corr ~ t1$numit, family = gaussian)
lines(p1)
p2 <- predict(t1gm, data.frame(4:18))
lines(p2)
plot(t1$numit, t1$corr, xlab="Number of additional constraints", ylab="Correlation")