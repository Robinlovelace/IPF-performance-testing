head(indf)

dim(indf)

head(indf[,,1,1]) # all individuals, all areas, constraint 1, it 1
head(indf[,,2,1]) # the integerisation process has begun!

#library(ggplot2)
#library(reshape2)

mindf1 <- melt(indf[,,2,1,1])
head(mindf1)
summary(mindf1)

mall.msim <- melt(all.msim)
head(mall.msim)
unique(mall.msim$variable)

plot(mall.msim[,2], mindf1[,3]) # This is the chunk of info needed for all plots
mall.msim$constraint <- c(rep("age/sex",24*12), rep("marriage", 24*5)
                          , rep("tenure", 24*5))
mindf4 <- melt(indf[,,,1,1])
mindf4 <- cbind(mindf4, 
                do.call("rbind", replicate(4, mall.msim, simplifyFALSE)))

summary(mindf4)
names(mindf4) <- c("observation", "Var2", "Var3", "simulated", "category",
                   "census",  "constraint")
mindf4$constraint <- as.factor(mindf4$constraint)

ggplot(mindf4[-which(mindf4$Var3 == 1),], aes(x=census, y=simulated, colourconstraint)) + 
  geom_point() + facet_grid(. ~ Var3)
ggsave("analysis1.png")

### Now do it for different weights!
head(wf[,,2,2,4])
wf.ws <- wf[3,1,1:3,,]
wf.ws.m <- melt(wf.ws)
head(wf.ws.m)
summary(wf.ws.m) # looks promising: var5 is weight
wf.ws.m$Var2as.factor(wf.ws.m$Var2)
p <- ggplot(wf.ws.m[,], aes(x=Var3, y=value, colour=paste(Var2,Var1)))
p + geom_line()

### Now do it for different individuals
head(wf[,,2,2,4])
ind.samp <- sample(1:nrow(ind),2)
ind.samp <- c(1654,1075)
wf.ws <- wf[c(50,12,ind.samp),2,1:3,,]
wf.ws.m <- melt(wf.ws)
head(wf.ws.m)
summary(wf.ws.m) # looks promising: var5 is weight
wf.ws.m$Var3as.factor(wf.ws.m$Var3)
wf.ws.m$Var1factor(wf.ws.m$Var1)
levels(wf.ws.m$Var1) <- as.character(c(1,2,ind.samp))
p <- ggplot(wf.ws.m[,], aes(x=Var4/5 + 0.5, y=value, colour=paste(Var3,Var2)))
p + geom_point() + geom_line() +
  xlab(label="Start weight") + ylab("Simulated weight") +
  facet_grid(facets= . ~Var1) + scale_color_discrete(name="Iteration") +
  theme_bw() 
#ggsave("weights-exp-5.4.nice2.pdf",width=20,height=10,units"cm")
