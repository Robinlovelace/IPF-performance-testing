# tmfdm - test measures on different models

measall <- read.csv("models/simple/measures.csv")
measall$Model <- "Simple"

m2 <- read.csv("models/small-area/measures.csv")
m2$Model <- "Small-area"

m3 <- read.csv("models/sheffield/measures.csv")
m3$Model <- "Sheffield"

measall <- rbind(measall, m2)
names(measall) <- names(m3)
measall <- rbind(measall, m3)
apply(measall, 2, class)
measall[-ncol(measall)] <- apply(measall[-ncol(measall)], 2, as.numeric)

library(dplyr)
mgroup <- group_by(measall, Model)
msum <- summarise_each(mgroup, funs = funs(mean))[rev(c(1,3,2)),]
msd <- summarise_each(mgroup, funs = funs(sd))[rev(c(1,3,2)),]

msum <- rbind(msum, msd)

library(knitr)
kable(msum[-c(2,3)], format = "latex", digits = 3)
