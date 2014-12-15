# create urban/rural constraint

con5 <- con1[1:2] * 0
names(con5) <- c("urban", "rural")
set.seed(2223)
sel <- sample(nrow(con1), nrow(con1) / 6)
con5$rural[sel] <- 1
con5$urban <- 1 - con5$rural

con5[] <- apply(con5, 2, function(x) x * rowSums(con1))
head(con5)
