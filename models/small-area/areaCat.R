category.labels # Create 0/1 counts from survey data

area.cat <- array(0,dim=c(nrow(intall[[i]]),length(category.labels !=0)))

area.cat[which(intall[[i]]$jbhrs >= 1 & intall[[i]]$jbhrs <6 & intall[[i]]$sex=="male"),1] <- 1 # 1-5
area.cat[which(intall[[i]]$jbhrs >= 6 & intall[[i]]$jbhrs <16 & intall[[i]]$sex=="male"),2] <- 1 
area.cat[which(intall[[i]]$jbhrs >= 16 & intall[[i]]$jbhrs <31 & intall[[i]]$sex=="male"),3] <- 1
area.cat[which(intall[[i]]$jbhrs >= 31 & intall[[i]]$jbhrs <38 & intall[[i]]$sex=="male"),4] <- 1  
area.cat[which(intall[[i]]$jbhrs >= 38 & intall[[i]]$jbhrs <49 & intall[[i]]$sex=="male"),5] <- 1  
area.cat[which(intall[[i]]$jbhrs >= 49 & intall[[i]]$sex=="male"),6] <- 1  
area.cat[which(intall[[i]]$jbhrs >= 1 & intall[[i]]$jbhrs <6 & intall[[i]]$sex=="female"),7] <- 1  
area.cat[which(intall[[i]]$jbhrs >= 6 & intall[[i]]$jbhrs <16 & intall[[i]]$sex=="female"),8] <- 1  
area.cat[which(intall[[i]]$jbhrs >= 16 & intall[[i]]$jbhrs <31 & intall[[i]]$sex=="female"),9] <- 1  
area.cat[which(intall[[i]]$jbhrs >= 31 & intall[[i]]$jbhrs <38 & intall[[i]]$sex=="female"),10] <- 1  
area.cat[which(intall[[i]]$jbhrs >= 38 & intall[[i]]$jbhrs <49 & intall[[i]]$sex=="female"),11] <- 1 
area.cat[which(intall[[i]]$jbhrs >= 49 & intall[[i]]$sex=="female"),12] <- 1 

# marriage status constraint
area.cat[which(intall[[i]]$marstat == "single"),13] <- 1
area.cat[which(intall[[i]]$marstat == "married"),14] <- 1   
area.cat[which(intall[[i]]$marstat == "separated"),15] <- 1
area.cat[which(intall[[i]]$marstat == "divorced"),16] <- 1      
area.cat[which(intall[[i]]$marstat == "widowed"),17] <- 1  

# tenure constraint
area.cat[which(intall[[i]]$house == "own"),18] <- 1  
area.cat[which(intall[[i]]$house == "mort"),19] <- 1   
area.cat[which(intall[[i]]$house == "shared"),20] <- 1  
area.cat[which(intall[[i]]$house == "letting"),21] <- 1
area.cat[which(intall[[i]]$house == "other"),22] <- 1

# Add more constraints here
# area.cat[which(intall[[i]]$dis >=  0 & intall[[i]]$dis <2 ),24] <- 1 
# area.cat[which(intall[[i]]$dis >= 2 & intall[[i]]$dis <5    ),25] <- 1 
# area.cat[which(intall[[i]]$dis >= 5 & intall[[i]]$dis < 10  ),26] <- 1
# area.cat[which(intall[[i]]$dis >= 10 & intall[[i]]$dis < 20 ),27] <- 1  
# area.cat[which(intall[[i]]$dis >= 20 & intall[[i]]$dis < 40 ),28] <- 1 
# area.cat[which(intall[[i]]$dis >= 40 & intall[[i]]$dis < 60 ),29] <- 1  
# area.cat[which(intall[[i]]$dis >= 60 ),30] <- 1  
# area.cat[which(intall[[i]]$dis < 0 ),31] <- 1 

# Polishing up
area.cat <- data.frame(area.cat)
names(area.cat) <- category.labels
head(area.cat)
summary(area.cat)
