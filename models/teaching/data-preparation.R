# Preparing the input data for spatial microsimulation

# Set your working directory to IPF-performance-testing
work_dir <- "~/repos/IPF-performance-testing"
setwd(work_dir) # set working directory to project's folder

# Loading the input data - individual data can be downloaded from here:
# http://www.ons.gov.uk/ons/guide-method/census/2011/census-data/census-microdata/microdata-teaching-file/index.html
# Once this has been downloaded (into the "/tmp/" folder below), unzip it
# unzip(zipfile = "/tmp/rft-teaching-file.zip", exdir = "/tmp/census/")

# TODO: create script to convert full .csv file

# Loading the individual-level teaching file
ind <- read.csv("/media/robin/data/ipf-vs-co/data/2011 Census Microdata Teaching File.csv")
head(ind)
summary(ind)

# Load in the aggregate constraints
con_age <- read.csv("data-big/uk-sars/QS103_Age.csv")[-1]
con_mar <- read.csv("data-big/uk-sars/LC1107_MaritalStatus.csv")[-1]
con_hrw <- read.csv("data-big/uk-sars/QS604_HoursWorked.csv")[-1]
con_loc <- read.csv("data-big/uk-sars/LC6112_LC6113_Occupation.csv")[-1]

cons <- cbind(con_mar, con_hrw, con_loc, con_age)
# cons <- cbind(con_mar, con_age, con_hrw, con_loc) # test impact of order
names(cons)[1:ncol(con_age)]

# Convert ind to model.matrix form
head(ind)
# Rename individual-level variables
names(ind) <- c("id", "region", "health", "age", "marital", "wrk_hrs", "occupation")

cat_mar <- model.matrix(~ ind$marital - 1)
# Check order
summary(ind$marital)
levels(ind$marital)
names(con_mar) # having loaded it after
colSums(cat_mar) # totals are right, order is wrong
cat_mar <- cat_mar[, c(2, 1, 4, 5, 3, 6)] # re-order cols manually
colnames(cat_mar) # check the colnames of cat_mar - too verbose
colnames(cat_mar) <- gsub("ind\\$marital", "", colnames(cat_mar)) # correct col names
names(con_mar)
cor(colSums(con_mar), colSums(cat_mar))

# Alternative way to re-order model matrix - testing
# ind$marital <- factor(ind$marital, levels = names(con_mar))
# cat_mar <- model.frame(~ ind$marital - 1)

cat_age <- model.matrix(~ ind$age - 1)
colSums(cat_age)
levels(ind$age) # confirmed: col order is same as levels
colSums(con_age) # seems correct
colnames(cat_age) <- gsub("ind\\$age", "", colnames(cat_age))
cor(colSums(cat_age), colSums(con_age))

cat_hrw <- model.matrix(~ ind$wrk_hrs - 1)
# ind$wrk_hrs <- factor(ind$wrk_hrs, levels = names()) # attempt to re-order factors
levels(ind$wrk_hrs)
colSums(con_hrw)
order(names(con_hrw))
o <- order(match(levels(ind$wrk_hrs), names(con_hrw))) # order of levels in constraints
cat_hrw <- cat_hrw[, o] # totally re-arranged
cor(colSums(con_hrw), colSums(cat_hrw))

cat_loc <- model.matrix(~ ind$occupation - 1)
colSums(cat_loc)
names(con_loc)
levels(ind$occupation) # wrong order
levels(ind$occupation)[c(1, 3:11, 2)]
o <- order(match(levels(ind$occupation), names(con_loc)))
cat_loc <- cat_loc[, o]
cor(colSums(con_loc), colSums(cat_loc))

ind_cat <- cbind(cat_mar, cat_hrw, cat_loc, cat_age)
# ind_cat <- cbind(cat_mar, cat_age, cat_hrw, cat_loc) # order test

names(ind_cat)
# colnames(ind_cat) <- c(levels(ind$marital), levels(ind$wrk_hrs), levels(ind$occupation), levels(ind$age))
# colnames(ind_cat) <- c(levels(ind$marital), levels(ind$age), levels(ind$wrk_hrs), levels(ind$occupation)) # order test

### Testing the column totals are correct
summary(ind$age)
# sum(ind$age == "A0")
colSums(ind_cat)

