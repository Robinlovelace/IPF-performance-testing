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
                do.call("rbind", replicate(4, mall.msim, simplify = FALSE)))

summary(mindf4)
names(mindf4) <- c("observation", "Var2", "Var3", "simulated", "category",
                   "census",  "constraint")
mindf4$constraint <- as.factor(mindf4$constraint)

ggplot(mindf4[-which(mindf4$Var3 == 1),], aes(x=census, y=simulated, colour = constraint)) + 
  geom_point() + facet_grid(. ~ Var3)
ggsave("analysis1.png")
### Now do it for multiple iterations YEAH!
