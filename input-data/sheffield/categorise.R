# converts numeric variables into categorical variables
category.labels # Create 0/1 counts from survey data

ind.cat <- array(rep(0,4697*666),dim=c(nrow(ind),length(category.labels !=0)))

ind.cat[which(ind$age <= 19 & ind$sex =="male"),1] <- 1 # 16-19yr olds, "m16_19"
  ind.cat[which(ind$age >= 20 & ind$age <= 24 & ind$sex =="male"),2] <- 1 #"m20_24"
  ind.cat[which(ind$age >= 25 & ind$age <= 34 & ind$sex =="male"),3] <- 1#"m25_34"
  ind.cat[which(ind$age >= 35 & ind$age <= 54 & ind$sex =="male"),4] <- 1 # "m35_54" 
  ind.cat[which(ind$age >= 55 & ind$age <= 59 & ind$sex =="male"),5] <- 1 #"m55_60"   
  ind.cat[which(ind$age >= 60 & ind$sex =="male"),6] <- 1  #[6] "m60_plus"
  ind.cat[which(ind$age >= 16 & ind$age <= 19 & ind$sex =="female"),7] <- 1 # "f16_19" 
  ind.cat[which(ind$age >= 20 & ind$age <= 24 & ind$sex =="female"),8] <- 1 #"f20_24"  
  ind.cat[which(ind$age >= 25 & ind$age <= 34 & ind$sex =="female"),9] <- 1 #"f25_34"  
  ind.cat[which(ind$age >= 35 & ind$age <= 54 & ind$sex =="female"),10] <- 1 # "f35_54" 
  ind.cat[which(ind$age >= 55 & ind$age <= 59 & ind$sex =="female"),11] <- 1 #[11] "f55_60"
  ind.cat[which(ind$age >= 60 & ind$sex =="female"),12] <- 1 #"f60_plus" 
  ind.cat[which(ind$mode == "na"),13] <- 1 #"mfh" 
  ind.cat[which(ind$mode == "Metro"),14] <- 1 #"metro"    
  ind.cat[which(ind$mode == "Train"),15] <- 1 #"train"
  ind.cat[which(ind$mode == "Bus"),16] <- 1 # [16] "bus"     
  ind.cat[which(ind$mode == "Moto"),17] <- 1 #"moto"  
  ind.cat[which(ind$mode == "Car (d)"),18] <- 1  #"car(d)
  ind.cat[which(ind$mode == "Car (p)"),19] <- 1   #"car(p)
  ind.cat[which(ind$mode == "Taxi"),20] <- 1 #"taxi"   
  ind.cat[which(ind$mode == "Cyc"),21] <- 1 #[21] "cycle" 
  ind.cat[which(ind$mode == "Walk"),22] <- 1 #"walk"  
  ind.cat[which(ind$mode == "Other"),23] <- 1 #"other"   
  # Including ncar (no, removed)
  
# Including dist
ind.cat[which(ind$dis >=  0 & ind$dis <2 ),24] <- 1 
ind.cat[which(ind$dis >= 2 & ind$dis <5    ),25] <- 1 
ind.cat[which(ind$dis >= 5 & ind$dis < 10  ),26] <- 1
ind.cat[which(ind$dis >= 10 & ind$dis < 20 ),27] <- 1  
ind.cat[which(ind$dis >= 20 & ind$dis < 40 ),28] <- 1 
ind.cat[which(ind$dis >= 40 & ind$dis < 60 ),29] <- 1  
ind.cat[which(ind$dis >= 60 ),30] <- 1  
ind.cat[which(ind$dis < 0 ),31] <- 1 

# Including nssec
ind.cat[which(ind$nssec == "large employers & higher management"),32] <- 1 
ind.cat[which(ind$nssec == "higher professional"),33] <- 1
ind.cat[which(ind$nssec == "lower management & professional"  ),34] <- 1 
ind.cat[which(ind$nssec == "intermediate"),35] <- 1
ind.cat[which(ind$nssec == "small employers & own account"),36] <- 1 
ind.cat[which(ind$nssec == "lower supervisory & technical"),37] <- 1
ind.cat[which(ind$nssec == "semi-routine"),38] <- 1 
ind.cat[which(ind$nssec == "routine" ),39] <- 1
ind.cat[which(ind$nssec == "Other" ),40] <- 1

# Polishing up
ind.cat <- data.frame(ind.cat)
names(ind.cat) <- category.labels
