for (j in 1:nrow(all.msim)){
  weights1[which(ind$age <= 19 & ind$sex =="male"),j] <- con1[j,1] /ind.agg[j,1] #16-19yr olds, "m16_19"
  weights1[which(ind$age >= 20 & ind$age <= 24 & ind$sex =="male"),j] <- con1[j,2] /ind.agg[j,2] #"m20_24"
  weights1[which(ind$age[] >= 25 & ind$age[] <= 34 & ind$sex[] =="male"),j] <- con1[j,3] /ind.agg[j,3] ## "m25_34"
  weights1[which(ind$age[] >= 35 & ind$age[] <= 54 & ind$sex[] =="male"),j] <- con1[j,4] /ind.agg[j,4] # "m35_54" 
  weights1[which(ind$age[] >= 55 & ind$age[] <= 59 & ind$sex[] =="male"),j] <- con1[j,5] /ind.agg[j,5] #"m55_60"   
  weights1[which(ind$age[] >= 60 & ind$sex[] =="male"),j] <- con1[j,6] /ind.agg[j,6]  #[6] "m60_plus"
  weights1[which(ind$age[] >= 16 & ind$age[] <= 19 & ind$sex[] =="female"),j] <- con1[j,7] /ind.agg[j,7] # "f16_19" 
  weights1[which(ind$age[] >= 20 & ind$age[] <= 24 & ind$sex[] =="female"),j] <- con1[j,8] /ind.agg[j,8] #"f20_24"  
  weights1[which(ind$age[] >= 25 & ind$age[] <= 34 & ind$sex[] =="female"),j] <- con1[j,9] /ind.agg[j,9] #"f25_34"  
  weights1[which(ind$age[] >= 35 & ind$age[] <= 54 & ind$sex[] =="female"),j] <- con1[j,10] /ind.agg[j,10] # "f35_54" 
  weights1[which(ind$age[] >= 55 & ind$age[] <= 59 & ind$sex[] =="female"),j] <- con1[j,11] /ind.agg[j,11] #[11] "f55_60"
  weights1[which(ind$age[] >= 60 & ind$sex[] =="female"),j] <- con1[j,12] /ind.agg[j,12] #"f60_plus"
}
############################################
#### Convert weights back into aggregate
#### values for each zone
############################################
for (i in 1:nrow(all.msim)){
  ind.agg1[i,]   <- colSums(ind.cat * weights1[,i])
}
# Test results for first row
ind.agg1[1,1:12] 
all.msim[1,1:12]

############################################
#### Second constraint - mode 
#### no source file used as it's short
############################################
for (j in 1:nrow(all.msim)){
  weights2[which(ind$mode == "na"),j] <- all.msim[j,13] /ind.agg1[j,13] # "mfh" 
  weights2[which(ind$mode == "na"),j] <- all.msim[j,13] /ind.agg1[j,13] # "mfh"
  weights2[which(ind$mode == "Metro"),j] <- all.msim[j,14] /ind.agg1[j,14] # "metro"   
  weights2[which(ind$mode == "Train"),j] <- all.msim[j,15] /ind.agg1[j,15] #"train" 
  weights2[which(ind$mode == "Bus"),j]  <- all.msim[j,16] /ind.agg1[j,16] #[16] "bus"     
  weights2[which(ind$mode == "Moto"),j] <- all.msim[j,17] /ind.agg1[j,17] #"Moto"   
  weights2[which(ind$mode == "Car (d)"),j] <- all.msim[j,18] /ind.agg1[j,18] #"car(d)"  
  weights2[which(ind$mode == "Car (p)"),j] <- all.msim[j,19] /ind.agg1[j,19] #"car(p)"  
  weights2[which(ind$mode == "Taxi"),j] <- all.msim[j,20] /ind.agg1[j,20] #"taxi" 
  weights2[which(ind$mode == "Cyc"),j] <- all.msim[j,21] /ind.agg1[j,21] #[21] "cycle"  
  weights2[which(ind$mode == "Walk"),j] <- all.msim[j,22] /ind.agg1[j,22] #"walk"  
  weights2[which(ind$mode == "Other"),j] <- all.msim[j,23] /ind.agg1[j,23] # "other"                      
}

############################################
#### Convert weights back into aggregate
#### values for each zone
############################################
for (i in 1:nrow(all.msim)){
  ind.agg2[i,]   <- colSums(ind.cat*weights1[,i] *weights2[,i])
}
# Test results for first row
ind.agg2[5,13:23] 
all.msim[5,13:23]

############################################
#### 3rd constraint: distance
############################################
for (i in 1:nrow(all.msim)){
  weights3[which(ind$dis < 2 & ind$dis >= 0 ),i] <- all.msim[i,24] /ind.agg2[i,24] # none
  weights3[which(ind$dis >= 2 & ind$dis <5    ),i] <- all.msim[i,25] /ind.agg2[i,25]
  weights3[which(ind$dis >= 5 & ind$dis < 10  ),i] <- all.msim[i,26] /ind.agg2[i,26]
  weights3[which(ind$dis >= 10 & ind$dis < 20 ),i] <- all.msim[i,27] /ind.agg2[i,27]
  weights3[which(ind$dis >= 20 & ind$dis < 40 ),i] <- all.msim[i,28] /ind.agg2[i,28]
  weights3[which(ind$dis >= 40 & ind$dis < 60 ),i] <- all.msim[i,29] /ind.agg2[i,29]
  weights3[which(ind$dis >= 60 ),i] <- all.msim[i,30] /ind.agg2[i,30]
  weights3[which(ind$dis < 0  ),i] <- all.msim[i,34] /ind.agg2[i,34] #na or mfh
}

############################################
#### Convert weights back into aggregate
#### values for each zone
############################################
weights4 <- weights0 * weights1 * weights2 * weights3 

for (i in 1:nrow(all.msim)){
  ind.agg3[i,] <- colSums(ind.cat * weights4[,i])
}

ind.agg3[5,13:29] 
all.msim[5,13:29]

cor(all.msim[,8],ind.agg3[,8]) # Should be reasonable (>.8)

############################################
#### 4th constraint: nssec
############################################
for (j in 1:nrow(all.msim)){
  weights4[which(ind$nssec8 == "large employers & higher management" ),j] <-
    all.msim[j,32] /ind.agg3[j,32] 
  weights4[which(ind$nssec8 == "higher professional" ),j] <-
    all.msim[j,33] /ind.agg3[j,33] 
  weights4[which(ind$nssec8 == "lower management & professional"),j]  <- all.msim[j,34] /ind.agg3[j,34]
  weights4[which(ind$nssec8 == "intermediate"),j]  <- all.msim[j,35] /ind.agg3[j,35]
  weights4[which(ind$nssec8 == "small employers & own account"    ),j]  <- all.msim[j,36] /ind.agg3[j,36]
  weights4[which(ind$nssec8 == "lower supervisory & technical" ),j]  <- all.msim[j,37] /ind.agg3[j,37]
  weights4[which(ind$nssec8 == "semi-routine"),j]  <- all.msim[j,38] /ind.agg3[j,38]
  weights4[which(ind$nssec8 == "routine"),j]  <- all.msim[j,39] /ind.agg3[j,39]
  weights4[which(ind$nssec8 == "Other"),j]  <- all.msim[j,40] /ind.agg3[j,40]
}
############################################
#### Convert weights back into aggregate
#### values for each zone
############################################
weights5 <- weights1 * weights2  * weights3 * weights4 
for (i in 1:nrow(all.msim)){
  ind.agg4[i,]   <- colSums(ind.cat * weights5[,i]) 
}

ind.agg4[5,30:40] 
all.msim[5,30:40]

### Save results for multiple iterations
# Save all useful values from last run
aagg <- data.frame(rbind(ind.agg1, ind.agg2, ind.agg3, ind.agg4))
aagg$cons <- rep(1:num.cons,rep(nrow(all.msim), num.cons))
head(aagg)

#i1.agg5 <- ind.agg5
#i1.agg6 <- ind.agg6

# Test results for first row
time1 <- proc.time() -start.time
time1

### Basic analysis
cor(all.msim[,8],ind.agg4[,8]) # Should be reasonable (>.8)
a.v <- as.vector(as.matrix(all.msim))
g.v <- as.vector(as.matrix(ind.agg4))
cor(a.v, g.v)

#### Iterations: uncomment following lines to run 10 iterations of IPF

# setwd("its/")
# source(file="etsim2.r") # Include to run second iteration
# source(file="etsim3.r") #
# source(file="etsim4.r") #
# source(file="etsim5.r") #
# source(file="etsim6.r") #
# source(file="etsim7.r") #
# source(file="etsim8.r") #
# source(file="etsim9.r") #
# source(file="etsim10.r") #
time10 <- proc.time() - start.time

## basu
plot(aagg[which(aagg$cons == 1),1], all.msim[,1])
plot(aagg[which(aagg$cons == 2),1:5], all.msim[,1:5])

plot(ind.agg2[,5], aagg[which(aagg$cons == 2),5])
