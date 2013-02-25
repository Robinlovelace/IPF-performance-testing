# categorise.R: script to convert numerical variables in to categories
# must be customised for different input constraints
category.labels # for humans: check the categories to be used

ind.cat <- array(0,dim=c(nrow(ind),length(category.labels !=0)))

ind.cat[which(ind$jbhrs >= 1 & ind$jbhrs <6 & ind$sex=="male"),1] <- 1 # 1-5
ind.cat[which(ind$jbhrs >= 6 & ind$jbhrs <16 & ind$sex=="male"),2] <- 1 
ind.cat[which(ind$jbhrs >= 16 & ind$jbhrs <31 & ind$sex=="male"),3] <- 1
ind.cat[which(ind$jbhrs >= 31 & ind$jbhrs <38 & ind$sex=="male"),4] <- 1  
ind.cat[which(ind$jbhrs >= 38 & ind$jbhrs <49 & ind$sex=="male"),5] <- 1  
ind.cat[which(ind$jbhrs >= 49 & ind$sex=="male"),6] <- 1  
ind.cat[which(ind$jbhrs >= 1 & ind$jbhrs <6 & ind$sex=="female"),7] <- 1  
ind.cat[which(ind$jbhrs >= 6 & ind$jbhrs <16 & ind$sex=="female"),8] <- 1  
ind.cat[which(ind$jbhrs >= 16 & ind$jbhrs <31 & ind$sex=="female"),9] <- 1  
ind.cat[which(ind$jbhrs >= 31 & ind$jbhrs <38 & ind$sex=="female"),10] <- 1  
ind.cat[which(ind$jbhrs >= 38 & ind$jbhrs <49 & ind$sex=="female"),11] <- 1 
ind.cat[which(ind$jbhrs >= 49 & ind$sex=="female"),12] <- 1 

# marriage status constraint
ind.cat[which(ind$marstat == "single"),13] <- 1
ind.cat[which(ind$marstat == "married"),14] <- 1   
ind.cat[which(ind$marstat == "separated"),15] <- 1
ind.cat[which(ind$marstat == "divorced"),16] <- 1      
ind.cat[which(ind$marstat == "widowed"),17] <- 1  

# tenure constraint
ind.cat[which(ind$house == "own"),18] <- 1  
ind.cat[which(ind$house == "mort"),19] <- 1   
ind.cat[which(ind$house == "shared"),20] <- 1  
ind.cat[which(ind$house == "letting"),21] <- 1
ind.cat[which(ind$house == "other"),22] <- 1
  
# Add more constraints here, e.g.
# ind.cat[which(ind$dis >=  0 & ind$dis <2 ),24] <- 1 

# Polishing up
ind.cat <- data.frame(ind.cat)
names(ind.cat) <- category.labels
