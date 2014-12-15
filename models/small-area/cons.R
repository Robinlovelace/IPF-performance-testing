### Constraint Variables ###
# this script reads-in constraint variable data files into R
# saved as a separate file to be modular and reduce size of scripts
# must be modified to read different constraint files

### con1 worked per week constraint
con1 <- read.table("input-data/small-area-eg/hrs_worked.csv", header=TRUE, sep=",")
names(con1) # Check names
con1 <- data.frame(m0 = con1$X1.5, m6 = con1$X6.15, m16 = con1$X16.30, 
                   m31 = con1$X31.37, m38 = con1$X38.48, m49 = con1$X49.or.more,
                   # now females
                   f0 = con1$X1.5.1, f6 = con1$X6.15.1, f16 = con1$X16.30.1, 
                   f31 = con1$X31.37.1, f38 = con1$X38.48.1, 
                   f49 = con1$X49.or.more.1
) # Now 12 variables
summary(con1)
names(con1)

### Marital status constraint
con2 <- read.table("input-data/small-area-eg/marital_status.csv", header=TRUE, sep=",")
names(con2)
con2 <- data.frame(single = con2$Single, married = con2$Married,
                   separated = con2$Separated, divorced = con2$Divorced, 
                   widowed = con2$Widowed)

### Tenure constraint
con3 <- read.table("input-data/small-area-eg/tenancy.csv", header=TRUE, sep=",")
names(con3)
con3$other <- con3$other + con3$council + con3$assoc
con3 <- data.frame(con3[,c(3,4,5,8,9)])
sapply(con3, mean) # for humans: check the loaded data makes sense
names(con3)



