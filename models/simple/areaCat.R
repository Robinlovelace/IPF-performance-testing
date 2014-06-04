category.labels # Create 0/1 counts from survey data

ind.cat <- array(0,dim=c(nrow(intall[[i]]),length(category.labels !=0)))

ind.cat[which(intall[[i]]$age < 50),1] <- 1 # Age, "< 50"
ind.cat[which(intall[[i]]$age >= 50),2] <- 1 # "50+"
ind.cat[which(intall[[i]]$sex =="m"),3] <- 1 # Sex constraint: "m" 
ind.cat[which(intall[[i]]$sex =="f"),4] <- 1 #"f"
sum(ind.cat) 

# Polishing up
area.cat <- data.frame(ind.cat)
names(ind.cat) <- category.labels
