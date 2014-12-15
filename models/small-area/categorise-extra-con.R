category.labels # Create 0/1 counts from survey data

ind.cat <- array(0,dim=c(nrow(ind), 30))

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

# Add more constraints here
# ind.cat[which(ind$dis >=  0 & ind$dis <2 ),24] <- 1 
# ind.cat[which(ind$dis >= 2 & ind$dis <5    ),25] <- 1 
# ind.cat[which(ind$dis >= 5 & ind$dis < 10  ),26] <- 1
# ind.cat[which(ind$dis >= 10 & ind$dis < 20 ),27] <- 1  
# ind.cat[which(ind$dis >= 20 & ind$dis < 40 ),28] <- 1 
# ind.cat[which(ind$dis >= 40 & ind$dis < 60 ),29] <- 1  
# ind.cat[which(ind$dis >= 60 ),30] <- 1  
# ind.cat[which(ind$dis < 0 ),31] <- 1 

age_ind <- cut(ind$age, breaks = c(0, 10, 20, 30, 50, 120))
summary(age_ind)
age_ind <- factor(paste(ind$sex, age_ind))
summary(age_ind)
age_cat <- as.data.frame(model.matrix( ~ age_ind -1))
ncol(age_cat)
names(age_cat)
age_cat <- age_cat[c(5:8, 1:4)] # ensure columns are in the right order
names(age_cat)
names(con4)
names(age_cat) <- names(con4)

# Polishing up
ind.cat <- data.frame(ind.cat)
names(ind.cat) <- category.labels

head(age_cat)
head(ind.cat)
ind.cat[,23:30] <- age_cat
head(ind.cat)
