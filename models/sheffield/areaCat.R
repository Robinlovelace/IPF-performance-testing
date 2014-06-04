# converts numeric variables into categorical variables
category.labels # Create 0/1 counts from survey data

ind.cat <- array(rep(0,4697*666),dim=c(nrow(intall[[i]]),length(category.labels !=0)))

ind.cat[which(intall[[i]]$age <= 19 & intall[[i]]$sex =="male"),1] <- 1 # 16-19yr olds, "m16_19"
  ind.cat[which(intall[[i]]$age >= 20 & intall[[i]]$age <= 24 & intall[[i]]$sex =="male"),2] <- 1 #"m20_24"
  ind.cat[which(intall[[i]]$age >= 25 & intall[[i]]$age <= 34 & intall[[i]]$sex =="male"),3] <- 1#"m25_34"
  ind.cat[which(intall[[i]]$age >= 35 & intall[[i]]$age <= 54 & intall[[i]]$sex =="male"),4] <- 1 # "m35_54" 
  ind.cat[which(intall[[i]]$age >= 55 & intall[[i]]$age <= 59 & intall[[i]]$sex =="male"),5] <- 1 #"m55_60"   
  ind.cat[which(intall[[i]]$age >= 60 & intall[[i]]$sex =="male"),6] <- 1  #[6] "m60_plus"
  ind.cat[which(intall[[i]]$age >= 16 & intall[[i]]$age <= 19 & intall[[i]]$sex =="female"),7] <- 1 # "f16_19" 
  ind.cat[which(intall[[i]]$age >= 20 & intall[[i]]$age <= 24 & intall[[i]]$sex =="female"),8] <- 1 #"f20_24"  
  ind.cat[which(intall[[i]]$age >= 25 & intall[[i]]$age <= 34 & intall[[i]]$sex =="female"),9] <- 1 #"f25_34"  
  ind.cat[which(intall[[i]]$age >= 35 & intall[[i]]$age <= 54 & intall[[i]]$sex =="female"),10] <- 1 # "f35_54" 
  ind.cat[which(intall[[i]]$age >= 55 & intall[[i]]$age <= 59 & intall[[i]]$sex =="female"),11] <- 1 #[11] "f55_60"
  ind.cat[which(intall[[i]]$age >= 60 & intall[[i]]$sex =="female"),12] <- 1 #"f60_plus" 
  ind.cat[which(intall[[i]]$mode == "na"),13] <- 1 #"mfh" 
  ind.cat[which(intall[[i]]$mode == "Metro"),14] <- 1 #"metro"    
  ind.cat[which(intall[[i]]$mode == "Train"),15] <- 1 #"train"
  ind.cat[which(intall[[i]]$mode == "Bus"),16] <- 1 # [16] "bus"     
  ind.cat[which(intall[[i]]$mode == "Moto"),17] <- 1 #"moto"  
  ind.cat[which(intall[[i]]$mode == "Car (d)"),18] <- 1  #"car(d)
  ind.cat[which(intall[[i]]$mode == "Car (p)"),19] <- 1   #"car(p)
  ind.cat[which(intall[[i]]$mode == "Taxi"),20] <- 1 #"taxi"   
  ind.cat[which(intall[[i]]$mode == "Cyc"),21] <- 1 #[21] "cycle" 
  ind.cat[which(intall[[i]]$mode == "Walk"),22] <- 1 #"walk"  
  ind.cat[which(intall[[i]]$mode == "Other"),23] <- 1 #"other"   
  # Including ncar (no, removed)
  
# Including dist
ind.cat[which(intall[[i]]$dis >=  0 & intall[[i]]$dis <2 ),24] <- 1 
ind.cat[which(intall[[i]]$dis >= 2 & intall[[i]]$dis <5    ),25] <- 1 
ind.cat[which(intall[[i]]$dis >= 5 & intall[[i]]$dis < 10  ),26] <- 1
ind.cat[which(intall[[i]]$dis >= 10 & intall[[i]]$dis < 20 ),27] <- 1  
ind.cat[which(intall[[i]]$dis >= 20 & intall[[i]]$dis < 40 ),28] <- 1 
ind.cat[which(intall[[i]]$dis >= 40 & intall[[i]]$dis < 60 ),29] <- 1  
ind.cat[which(intall[[i]]$dis >= 60 ),30] <- 1  
ind.cat[which(intall[[i]]$dis < 0 ),31] <- 1 

# Including nssec
ind.cat[which(intall[[i]]$nssec8 == "large employers & higher management"),32] <- 1 
ind.cat[which(intall[[i]]$nssec8 == "higher professional"),33] <- 1
ind.cat[which(intall[[i]]$nssec8 == "lower management & professional"  ),34] <- 1 
ind.cat[which(intall[[i]]$nssec8 == "intermediate"),35] <- 1
ind.cat[which(intall[[i]]$nssec8 == "small employers & own account"),36] <- 1 
ind.cat[which(intall[[i]]$nssec8 == "lower supervisory & technical"),37] <- 1
ind.cat[which(intall[[i]]$nssec8 == "semi-routine"),38] <- 1 
ind.cat[which(intall[[i]]$nssec8 == "routine" ),39] <- 1
ind.cat[which(intall[[i]]$nssec8 == "Other" ),40] <- 1

# Polishing up
area.cat <- data.frame(ind.cat)
names(area.cat) <- category.labels

