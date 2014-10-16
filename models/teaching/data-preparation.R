# Preparing the input data for spatial microsimulation

# Set your working directory to IPF-performance-testing
work_dir <- "~/repos/IPF-performance-testing"
setwd(work_dir) # set working directory to project's folder

# Loading the input data - individual data can be downloaded from here:
# http://www.ons.gov.uk/ons/guide-method/census/2011/census-data/census-microdata/microdata-teaching-file/index.html
# Once this has been downloaded (into the "/tmp/" folder below), unzip it
unzip(zipfile = "/tmp/rft-teaching-file.zip", exdir = "/tmp/census/")

# TODO: create script to convert full .csv file

# Loading the teaching file
ind <- read.csv("/media/robin/data/ipf-vs-co/data/2011 Census Microdata Teaching File.csv")
head(ind)
summary(ind)

# Convert to model.matrix form
head(ind)

cat_mar <- model.matrix(~ ind$LC1107 - 1)
# Check order
summary(ind$LC1107)
levels(ind$LC1107)
names(con_mar) # having loaded it after
colSums(cat_mar) # totals are right, order is wrong
cat_mar <- cat_mar[, c(2, 1, 4, 5, 3, 6)]

cat_age <- model.matrix(~ ind$QS103 - 1)
colSums(cat_age)
levels(ind$QS103) # confirmed: col order is same as levels
colSums(con_age) # seems correct

cat_hrw <- model.matrix(~ ind$QS604 - 1)
levels(ind$QS604)
colSums(con_hrw)
order(names(con_hrw))
o <- order(match(levels(ind$QS604), names(con_hrw))) # order of levels in constraints
cat_hrw <- cat_hrw[, o] # totally re-arranged

cat_loc <- model.matrix(~ ind$LC6112_LC6113 - 1)
colSums(cat_loc)
names(con_loc)
levels(ind$LC6112_LC6113) # wrong order
levels(ind$LC6112_LC6113)[c(1, 3:11, 2)]
o <- order(match(levels(ind$LC6112_LC6113), names(con_loc)))
cat_loc <- cat_loc[, o]

ind_cat <- cbind(cat_mar, cat_hrw, cat_loc, cat_age)
# ind_cat <- cbind(cat_mar, cat_age, cat_hrw, cat_loc) # order test

names(ind_cat)
colnames(ind_cat) <- c(levels(ind$LC1107), levels(ind$QS604), levels(ind$LC6112_LC6113), levels(ind$QS103))
# colnames(ind_cat) <- c(levels(ind$LC1107), levels(ind$QS103), levels(ind$QS604), levels(ind$LC6112_LC6113)) # order test

### Testing the column totals are correct
summary(ind$QS103)
# sum(ind$QS103 == "A0")
colSums(ind_cat)

# Load in the aggregate constraints
con_age <- read.csv("data-big/uk-sars/QS103_Age.csv")[-1]
con_mar <- read.csv("data-big/uk-sars/LC1107_MaritalStatus.csv")[-1]
con_hrw <- read.csv("data-big/uk-sars/QS604_HoursWorked.csv")[-1]
con_loc <- read.csv("data-big/uk-sars/LC6112_LC6113_Occupation.csv")[-1]

cons <- cbind(con_mar, con_hrw, con_loc, con_age)
# cons <- cbind(con_mar, con_age, con_hrw, con_loc) # test impact of order
names(cons)
