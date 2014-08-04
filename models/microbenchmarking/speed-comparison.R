library(microbenchmark)

microbenchmark(source("models/simple/etsim.R"), source("models/simple/etsim-ipfp.R"), times = 3)
