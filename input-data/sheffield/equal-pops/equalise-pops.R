head(con1)
head(con2)
con1 <- round(con1)
head(con1)
con2 <- round(con2)
con3 <- round(con3)
con4 <- round(con4)

sum(con1); sum(con2); sum(con3); sum(con4)

write.csv(con1, "input-data/sheffield/equal-pops/age-sex.csv", row.names=F)
write.csv(con2, "input-data/sheffield/equal-pops/mode.csv", row.names=F)
write.csv(con3, "input-data/sheffield/equal-pops/dist.csv", row.names=F)
write.csv(con4, "input-data/sheffield/equal-pops/ns_sec.csv", row.names=F)

head(all.msim)
head(ind)
load("/tmp/ind.RData")
