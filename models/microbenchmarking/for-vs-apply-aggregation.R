

# Microbenchmarking speed testing of aggregation functions ####################
# Run this after "etsim.R" and "etsim-ipfp.R" in models/simple/"
source("models/simple/etsim.R")
source("models/simple/etsim-ipfp.R")

# test that the results of the two operations are the same...
ind_agg_ipf == t(apply(weights_ipf, MARGIN = 2, FUN = function(x) colSums(x * ind.cat)))

agg_method_loop <- function(){  for (i in 1:nrow(all.msim)){ # convert con1 weights back into aggregates
  ind_agg_ipf[i,] <- colSums(ind.cat * weights_ipf[,i])
}
}
agg_method_apply <- function() { t(apply(weights_ipf, MARGIN = 2, FUN = function(x) colSums(x * ind.cat))) }

library(microbenchmark)

time_agg_methods <- microbenchmark(agg_method_loop(), agg_method_apply)
print(time_agg_methods)[1,3] / print(time_agg_methods)[2,3]
# apply method is 5 orders of magnitude faster than for method!

