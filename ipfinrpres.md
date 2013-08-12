% Evaluating the performance of IPF: new tests for an old technique
% Robin Lovelace
% [RSAI-BIS](http://www.rsai-bis.org/) August 2013

# Introduction 

- Iterative proportional fitting is a long-established statistical technique [(Deming, 1940)](http://www.jstor.org/stable/10.2307/2235722)
- It estimates the values of internal cells, based on marginal totals:


This page showcases an updated version of the model `etsim.R` in action.
The ultimate aim of these improvements is to set all the parameters in the first few lines of code, dramatically reducing the time spent fiddling about with large bodies of code to get the code working correctly.

Note also the final correlation is 1, indicating that the IPF code is working correctly and converging to the optimal solution.



```r
# Initial conditions
start.time <- proc.time() # for measuring model runtime
l = 1
# for(l in 1:1){
k = l * 1
num.its = 3

# set working directory of input data
setwd("~/IPF-performance-testing/input-data/small-area-eg/") 

load("ind.RData")  # read-in the survey dataset. Preprocessed to 
# to contain the desired microdataset and saved in a dataframe called 'ind'

# read aggregate constraints. nrow of these data frames (areas) must be equal 
source(file="cons.R") # call separate script to read in data, for modularity
# calculate number of constraints (objects with names con1, con2 etc):
num.cons <- length(grep(pattern="con[1-9]", x=ls())) 

# Checking that totals add up
sum(con1)
```

```
## [1] 2785
```

```r
sum(con2)
```

```
## [1] 4404
```

```r
sum(con3) # add more if need's be
```

```
## [1] 2496
```

```r

all.msim <- cbind(con1 
                  # comment out constraints not included
                  ,con2
                  ,con3
                  #,con4 # add more constraints here if needed
                  )

# setting totals to sum(con2) - replace con2 with most reliable constraint
con.pop <- rowSums(con2) 

con1 <- con1 * con.pop / rowSums(con1)
con2 <- con2 * con.pop / rowSums(con2) 
con3 <- con3 * con.pop / rowSums(con3)

sum(con1) == sum(con2) 
```

```
## [1] TRUE
```

```r
sum(con2) == sum(con3)
```

```
## [1] TRUE
```

```r

# IF CELL VALUE == 0, SET to 0.0001 (a very small number)
con1[con1 == 0]   <- 0.0001 
con2[con2 == 0]   <- 0.0001 
con3[con3 == 0]   <- 0.0001 # add more constraints if needed

# setting-up reweighting data
category.labels <- names(all.msim) # should be correct from cons.R
all.mim.orig <- all.msim # save original (un-adjusted) constraints
all.msim <- cbind(con1 
                  ,con2
                  ,con3
                  #,con4 # add more if needed
                  )

# aggregate values - column for each category
source("categorise.R") # this script must be customised to input data

# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct
```

```
## [1] TRUE
```

```r
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 
```

```
## [1] TRUE
```

```r
sum(ind.cat[,ncol(con1)+ncol(con2)+1:ncol(con3)]) == nrow(ind) 
```

```
## [1] TRUE
```

```r

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(all.msim),num.cons+1)) 
weights[,,4][] <- 1 # sets initial weights to 1
weights[8,,4] <- k
ini.ws <- weights[,,4]

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim=c(nrow(all.msim),ncol(all.msim),num.cons+1))

for (i in 1:nrow(all.msim)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[i,1,4]}

# re-weighting for constraint 1 via IPF 
for (j in 1:nrow(all.msim)){
 weights[which(ind.cat[,1] == 1),j,1] <- con1[j,1] /ind.agg[j,1,1]
 weights[which(ind.cat[,2] == 1),j,1] <- con1[j,2] /ind.agg[j,2,1]
 weights[which(ind.cat[,3] == 1),j,1] <- con1[j,3] /ind.agg[j,3,1]
 weights[which(ind.cat[,4] == 1),j,1] <- con1[j,4] /ind.agg[j,4,1]
 weights[which(ind.cat[,5] == 1),j,1] <- con1[j,5] /ind.agg[j,5,1]
 weights[which(ind.cat[,6] == 1),j,1] <- con1[j,6] /ind.agg[j,6,1]
 weights[which(ind.cat[,7] == 1),j,1] <- con1[j,7] /ind.agg[j,7,1]
 weights[which(ind.cat[,8] == 1),j,1] <- con1[j,8] /ind.agg[j,8,1]
 weights[which(ind.cat[,9] == 1),j,1] <- con1[j,9] /ind.agg[j,9,1]
 weights[which(ind.cat[,10] == 1),j,1] <- con1[j,10] /ind.agg[j,10,1]
 weights[which(ind.cat[,11] == 1),j,1] <- con1[j,11] /ind.agg[j,11,1]
 weights[which(ind.cat[,12] == 1),j,1] <- con1[j,12] /ind.agg[j,12,1]}

# convert con1 weights back into aggregates
for (i in 1:nrow(all.msim)){
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,4] * weights[,i,1])}

# test results for first row
ind.agg[1,1:12,2] - all.msim[1,1:12]
```

```
##   m1 m6       m16 m31 m38 m49 f1         f6        f16 f31 f38 f49
## 1  0  0 8.882e-16   0   0   0  0 -1.776e-15 -3.553e-15   0   0   0
```

```r

# second constraint
for (j in 1:nrow(all.msim)){
  weights[which(ind.cat[,13] == 1),j,2] <- all.msim[j,13] /ind.agg[j,13,2]
  weights[which(ind.cat[,14] == 1),j,2] <- all.msim[j,14] /ind.agg[j,14,2]
  weights[which(ind.cat[,15] == 1),j,2] <- all.msim[j,15] /ind.agg[j,15,2]
  weights[which(ind.cat[,16] == 1),j,2] <- all.msim[j,16] /ind.agg[j,16,2]
  weights[which(ind.cat[,17] == 1),j,2] <- all.msim[j,17] /ind.agg[j,17,2]}  

# convert con2 back into aggregate
for (i in 1:nrow(all.msim)){
ind.agg[i,,3]   <- colSums(ind.cat * weights[,i,4] * weights[,i,1] *
                             weights[,i,2])}

# test results for first row
ind.agg[5,ncol(con1)+1:ncol(con2),3] 
```

```
## [1]  43 118   5  27  15
```

```r
all.msim[5,ncol(con1)+1:ncol(con2)]
```

```
##   single married separated divorced widowed
## 5     43     118         5       27      15
```

```r

# third constraint
for (j in 1:nrow(all.msim)){
  weights[which(ind.cat[,18] == 1),j,3] <- all.msim[j,18] /ind.agg[j,18,3]
  weights[which(ind.cat[,19] == 1),j,3] <- all.msim[j,19] /ind.agg[j,19,3]
  weights[which(ind.cat[,20] == 1),j,3] <- all.msim[j,20] /ind.agg[j,20,3]
  weights[which(ind.cat[,21] == 1),j,3] <- all.msim[j,21] /ind.agg[j,21,3]
  weights[which(ind.cat[,22] == 1),j,3] <- all.msim[j,22] /ind.agg[j,22,3]}  

# convert con3 back into aggregate
for (i in 1:nrow(all.msim)){
  ind.agg[i,,4]   <- colSums(ind.cat * weights[,i,4] * weights[,i,1] * weights[,i,2] * 
                               weights[,i,3])}
# test results for first row
ind.agg[5,ncol(con1)+ncol(con2)+1:ncol(con3),4] 
```

```
## [1] 92.2435 88.6261  0.0001 12.6609 14.4696
```

```r
all.msim[5,ncol(con1)+ncol(con2)+1:ncol(con3)]
```

```
##     own  mort shared letting other
## 5 92.24 88.63  1e-04   12.66 14.47
```

```r

# for multiple iterations
wf <- array(dim=c(dim(weights), num.its))
indf <- array(dim=c(dim(ind.agg), num.its))
wf[,,,1] <- weights 
indf[,,,1] <- ind.agg

a.v <- as.vector(as.matrix(all.msim)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,4,1]))
t1 <- data.frame(it = 1, corr = cor(a.v,g.v))
```


### Now run the loop as many times as specified by num.its
This code has been massively shrunk compared with previous editions, and now requires only 1 external file, `e2.R`. Note also that the code also automatically generates a simple evaluation and plots it at the end:


```r
for (it in 2:num.its) {
    source(file = "~/IPF-performance-testing/input-data/small-area-eg/e2.R")
    wf[, , , it] <- weights
    indf[, , , it] <- ind.agg
    g.v <- as.vector(as.matrix(indf[, , 4, it]))
    t1[it, ] <- c(it, cor(a.v, g.v))
}
barplot(height = t1$corr, names.arg = t1$it, ylim = c(t1[1, 2], 1))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
t1
```

```
##   it   corr
## 1  1 0.9982
## 2  2 1.0000
## 3  3 1.0000
```



