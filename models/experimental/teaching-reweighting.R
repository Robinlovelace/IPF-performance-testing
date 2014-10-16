# Set your working directory to IPF-performance-testing
work_dir <- "D:/user/IPF-performance-testing-master/" 
setwd(work_dir)
# loading the SARS file
ind <- read.csv("2011 Census Microdata Teaching File.csv")
summary(ind)

# Convert to model.matrix form
head(ind)

cat_mar <- model.matrix(~ ind$LC1107 - 1)
# is this in the right order?
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
o <- order(match(levels(ind$QS604), names(con_hrw)))
cat_hrw <- cat_hrw[, o] # totally re-arranged

cat_loc <- model.matrix(~ ind$LC6112_LC6113 - 1)
colSums(cat_loc)
names(con_loc)
levels(ind$LC6112_LC6113) # wrong order
levels(ind$LC6112_LC6113)[c(1, 3:11, 2)]
o <- order(match(levels(ind$LC6112_LC6113), names(con_loc)))
o
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

con_age <- read.csv("data-big/uk-sars/QS103_Age.csv")[-1]
con_mar <- read.csv("data-big/uk-sars/LC1107_MaritalStatus.csv")[-1]
con_hrw <- read.csv("data-big/uk-sars/QS604_HoursWorked.csv")[-1]
con_loc <- read.csv("data-big/uk-sars/LC6112_LC6113_Occupation.csv")[-1]

cons <- cbind(con_mar, con_hrw, con_loc, con_age)
# cons <- cbind(con_mar, con_age, con_hrw, con_loc) # test impact of order
names(cons)
ind_agg <- cons # create aggregated estimated outputs (same dims and cons)
cons <- apply(cons, 2, as.numeric)
cons <- cons[1:20, ] # start with small version of the constraints

# Function to create count version of ind
# indp <- apply(ind_cat, 1, paste0, collapse = "") # "pasted" version of constraints

umat_count <- function(x, xp) {
  freq <- table(xp) # frequency of occurence of each individual
  xu <- unique(x) # save only unique individuals
  rns <- as.integer(row.names(xu)) # save the row names of unique values of ind
  xpu <- xp[rns]
  o <- order(xpu, decreasing = TRUE) # the order of the output (to rectify table)
  ind_num <- freq[o]
  cbind(xu, data.frame(ind_num, rns)) # outputs
}

# alternative to above
ind_cat <- data.frame(ind_cat)
library(plyr)
library(dplyr)

# x <- ind_cat
# x$p <- apply(ind_cat, 1, paste0, collapse = "")

umat_count_dplyr <- function(x){
  x$p <- apply(x, 1, paste0, collapse = "")
  up <- data.frame(p = unique(x$p)) # unique values in order they appeared
  y <- dplyr::count(x, p) # fast freq table
  umat <- inner_join(up, y) # quite fast
  umat <- join(umat, x, match = "first")
  list(u = umat, p = x$p) # return unique individuals and attributes
  }

umat <- umat_count_dplyr(ind_cat)
head(umat$u[1:5])

indu <- umat$u[-(1:2)] # non-count version - just * umat$freq
# indu <- apply(umat[1:ncol(ind_cat)], 2, function(x) x * umat$ind_num) # unique counts
# head(indu)
ind_test <- indu[rep(row.names(umat$u), umat$u$n),] # we've returned full circle to the correct population
head(ind_test[1:10])
colSums(ind_test) - colSums(ind_cat) # getting the right results!
# input for ipfp
library(ipfp)
A <- t(indu)
x0 <- rep(1, nrow(indu))

i = 1 # preparing for for loop

w <- ipfp(cons[i, ], A, x0, maxit = 20) # ipfp on 1st constraint
summary(w)
weights <- w / umat$u$n # to go from per category to per person weights
weights <- weights[rep(1:nrow(umat$u), times = umat$u$n)] # final weights
source("~/Dropbox/spatial-microsim-book/R/functions.R")

# in a weight matrix
weights <- apply(cons, 1, function(x) ipfp(x, A, x0, maxit = 20))

# We get a better correlation after spatial microsim - only slightly due to tiny weights
summary(weights)
sum(weights)
iweights <- int_trs(weights[,1])
length(iweights)
colSums(ind_cat[iweights,])
cor(cons[1,], colSums(ind_cat[iweights,]))
cor(cons[1,], colSums(ind_cat)) # far better than pre-re-arrangement

# Alternative: For loop to extract ind. indices from uniqe weights
library(sfsmisc)
sfsmisc::roundfixS(c(0.5, 4, 3, 9.5)) # testing the function
iw <- roundfixS(weights)
index <- NULL
for(i in 1:nrow(umat$u)){
  sel <-  which(umat$p == umat$u$p[i])
  #   head(ind_cat[sel,1:7]) # test it works: remove
  index <- c(index, sample(sel, size = iw[i], replace = TRUE))
}
ind_agg_test <- colSums(ind_cat[index, ])
cor(cons[1,], ind_agg_test)
cor(cons[1,], colSums(ind_cat)) # new method is tested and working - data issues


# in for loop of all zones
ids <- as.list(rep(NA, nrow(cons)))
ind_agg <- NULL
for(j in 1:nrow(cons)){
  iw <- roundfixS(weights[,j])
  index <- NULL
  for(i in 1:nrow(umat$u)){
    sel <-  which(umat$p == umat$u$p[i])
    #   head(ind_cat[sel,1:7]) # test it works: remove
    index <- c(index, sample(sel, size = iw[i], replace = TRUE))
  }
  ids[[j]] <- index
  ind_agg <- rbind(ind_agg, colSums(ind_cat[index, ]))
}
head(ind_agg)
cor(as.numeric(as.matrix(ind_agg)), as.numeric(as.matrix(cons)))
# cor > 0.999: not bad

# The above is everything that's needed to get cracking on the write-up of this for a paper, a book and the Maastricht conference











# function to see how well the model matrix version fits reality
fun <- function(par, ind_num, con){
  sim <- colSums(par * ind_num)
  ae <- abs(sim - con) # Absolute error per category
  sum(ae) # the Total Absolute Error (TAE)
}

fun(w, indu, cons[1,])


