### Constraint Variables ###
age.const <- read.csv("age-sex.csv", header=TRUE, sep=",")[103:173,]
names(age.const) # Check names

# Sort data frame to ensure sorted by MSOA code
age.const$mnemonic <- as.vector(age.const$mnemonic)
age.const.sort<-age.const[order(age.const$mnemonic),]
age.const <- age.const.sort
rm(age.const.sort)

# Economically active males
m16_19 <- age.const$m16+age.const$m18
m20_24 <- age.const$m20
m25_34 <- age.const$m25
m35_54 <- age.const$m35
m55_60 <- age.const$m55
m60_plus <- age.const$m60 + age.const$m65

# Economically active females
f16_19 <- age.const$f16+age.const$f18
f20_24 <- age.const$f20
f25_34 <- age.const$f25
f35_54 <- age.const$f35
f55_60 <- age.const$f55
f60_plus <- age.const$f60 + age.const$f65

# Check totals - won't add up; scrambled data!
sum(age.const$f_total)
sum(f16_19+f20_24+f25_34+f35_54+f55_60+f60_plus)
sum(age.const$all_ecact)
sum(m16_19+m20_24+m25_34+m35_54+m55_60+m60_plus+age.const$f_total) 

# Setting up msim table - matrix with 12 data columns
con1 <- cbind(m16_19,m20_24,m25_34,m35_54,m55_60,m60_plus,
                  f16_19,f20_24,f25_34,f35_54,f55_60,f60_plus)

# Census data (con1) now ready to re-weight survey

rm(m16_19,m20_24,m25_34,m35_54,m55_60,m60_plus,
                  f16_19,f20_24,f25_34,f35_54,f55_60,f60_plus)





### Constraint Variables ####################################################
mode.const <- read.csv("mode.csv", header=TRUE)[103:173,]
names(mode.const) # Check names

# rename columns
names(mode.const) <- c("zcode","name","mfh","metro","train","bus","moto",
                       "car.d", "car.p", "taxi","cycle", "walk","other", "avdist")

# Sort data frame to ensure sorted by MSOA code
mode.const$zcode <- as.vector(mode.const$zcode)
mode.const.sort<-mode.const[order(mode.const$zcode),]
mode.const <- mode.const.sort

# save distance columns, remove superfluous data
avdist <- mode.const$avdist

mode.const.clean <- cbind(mode.const[,c(3:13)])
con2 <- as.matrix(mode.const.clean)
rm(mode.const.clean)


# Check totals
sum(con2)


# Remove superfluous data
rm(mode.const.sort)

############################ Distance ####################
dist.const <- read.csv("dist.csv", header=TRUE)[103:173,]
names(dist.const)
dist.const[,1] <- as.vector(dist.const[,1])
dist.const.sort<-dist.const[order(dist.const[,1]),]
dist.const <- dist.const.sort
rm(dist.const.sort)
dist.const <- dist.const[,3:10]
head(dist.const)
names(dist.const)<-c("_2","_5","_10","_20","_40","_60","_61+","na-mfh")
con3 <- dist.const

head(dist.const)

### Constraint Variable -ns_sec ###
nssec.const <- read.csv("ns_sec.csv", header=TRUE)[103:173,]
names(nssec.const) # Check names

# rename columns
names(nssec.const) -> nssecnames

names(nssec.const) <- c("zcode","name","T","nssec_1","nssec_1.2","nssec_2","nssec_3","nssec_4","nssec_5", "nssec_6", "nssec_7","nssec_8", "FTS","Other")
# Combine full time student and 'other' categories
nssec.const <- nssec.const[,1:14] # remove superfluous 'other'

# Sort data frame to ensure sorted by MSOA code
nssec.const$zcode <- as.vector(nssec.const$zcode)
nssec.const.sort<-nssec.const[order(nssec.const$zcode),]
nssec.const <- nssec.const.sort

# save distance columns, remove superfluous data
nssec.const.clean <- cbind(nssec.const[,c(4:11,14)])
con4 <- as.matrix(nssec.const.clean)










