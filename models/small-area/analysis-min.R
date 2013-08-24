# analysis of impact of initial weights - simplest case (after etsim-min.R)
s.w # individual weight
smelted1 <- melt(wf[s.w,1:2,1:3,,1])
smelted2 <- melt(wf[s.w,1:2,1:3,,2])
summary(smelted1)
(smelted1[which(smelted1$Var2 == 3),])
smelted1$difs <-  sqrt((smelted1$value - smelted2$value)^2)
smelted1$dif <-  (smelted1$value - smelted2$value)
smelted1$difsp <- smelted1$difs / smelted1$value
smelted1$difp <- smelted1$dif / smelted1$value
smelted1$it <- smelted1$Var3 + (smelted1$Var2 - 1)/3
plot(smelted1$it, smelted1$dif)
plot(smelted1$it, smelted1$difs)
plot(smelted1$it, smelted1$difp)

qplot(data=smelted1, x=it, y=difp, colour=as.character(Var2)) + geom_line() + facet_grid(. ~ Var1)

# plotting for all areas, only final value (simpler)
smelted1 <- melt(wf[s.w,,1:3,,1])
smelted2 <- melt(wf[s.w,,1:3,,2])
smelted1$difs <-  sqrt((smelted1$value - smelted2$value)^2)
smelted1$dif <-  (smelted1$value - smelted2$value)
smelted1$difsp <- smelted1$difs / smelted1$value
smelted1$difp <- smelted1$dif / smelted1$value
smelted1$it <- smelted1$Var3 + (smelted1$Var2 - 1)/3
sm.one <- smelted1[which(smelted1$Var2 == 3),]
summary(sm.one)
names(sm.one)[3] <- "iteration"

p <- ggplot(data=sm.one, aes(x=iteration, y=difp, colour=as.character(Var1))) 
p + geom_line() + scale_color_discrete(legend=F) + ylab("Difference (proportion)")

smelted1 <- melt(wf[570,,1:3,,1])
smelted2 <- melt(wf[570,,1:3,,2])
smelted1$difs <-  sqrt((smelted1$value - smelted2$value)^2)
smelted1$dif <-  (smelted1$value - smelted2$value)
smelted1$difsp <- smelted1$difs / smelted1$value
smelted1$difp <- smelted1$dif / smelted1$value
smelted1$it <- smelted1$Var3 + (smelted1$Var2 - 1)/3
sm.two <- smelted1[which(smelted1$Var2 == 3),]

sm.two$individual <- 570
sm.one$individual <- s.w
names(sm.two)[3] <- "iteration"
sm.three <- rbind(sm.one, sm.two)

p <- ggplot(data=sm.three, aes(x=iteration, y=difp, colour=as.character(Var1))) 
p + geom_line() + scale_color_discrete(guide="none") + 
  ylab("Difference (proportion)") + facet_grid(. ~ individual) + theme_bw()

ggsave("weight-1-5-its.png", width=8)
ggsave("weight-1-5-its.pdf", width=8)
