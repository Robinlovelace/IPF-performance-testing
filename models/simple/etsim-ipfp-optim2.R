# IPF example implemented using the ipfp package
start_time <- Sys.time()

# Read-in data (manually to start, will use scripts in future)
num.its <- 10
c.names <- c("id", "age", "sex")
ind <- c(       1, "50+", "m",
                2, "50+", "m", 
                3, "16-49", "m", 
                4, "50+", "f", 
                5, "16-49", "f")
ind <- matrix(ind, nrow = 5, byrow = T) # Convert long data into matrix, by row
ind <- data.frame(ind) # Convert this into a dataframe
names(ind) <- c.names # Add correct column names

# Read in the data in constraints
category.labels <- c("16-49", "50+",  "m", "f") # Age, and sex constraints
all.msim <- c(        8,        4,     6,    6,
                      2,        8,     4,    6,  
                      7,        4,     3,    8,   
                      5,        4,     7,    2,   
                      7,        3,     6,    4,   
                      5,        3,     2,    6  
) # each row represents an area, each column a category
all.msim <- matrix(all.msim, ncol = 4, byrow = T) # Convert long data into matrix, by row

ind.cat <- data.frame(cbind(model.matrix(~ind$age - 1), 
  model.matrix(~ind$sex - 1)[,c(2,1)]))
names(ind.cat) <- category.labels
ind.agg <- all.msim
ind.agg[] <- NA

## Subsetting the ind variable
code <- do.call(paste, c(ind.cat, sep = ""))
codeU <- code[!duplicated(code)]
ind.cat.u <- ind.cat[!duplicated(code), ]

A <- t(ind.cat.u) # the constraint matrix for ipfp
x0 <- rep(1, nrow(ind.cat.u))

# find weights for all areas
library(ipfp)
for(i in 1:nrow(all.msim)){
  y <- as.numeric(all.msim[i,]) # the constraint vector to be emulated
  w <- ipfp(y, A, x0, verbose = F, maxit = num.its) ## got to here!!!
  ind.agg[i,] <- colSums()
}

# re-aggregate to aggregate level

## The old way...
# for (i in 1:nrow(all.msim)){ # convert con1 weights back into aggregates
#   ind_agg_ipf[i,] <- colSums(ind.cat * weights_ipf[,i])
# }

# The new way: alternate re-aggregation strategy
ind_agg_ipf <- t(apply(weights_ipf, MARGIN = 2, FUN = function(x) colSums(x * ind.cat)))

(time_taken_ipf <- Sys.time() - start_time )
# as.numeric(time_taken) / as.numeric(time_taken_ipf) # compare with basic etsim code

## Comparison of result from R code and ipfp implementation
# ind_agg_ipf[1,]
# all.msim[1,]
# ind.agg[,,3]
# ind_agg_ipf - all.msim # it works


