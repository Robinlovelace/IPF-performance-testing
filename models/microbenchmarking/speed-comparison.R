library(microbenchmark)

microbenchmark(source("models/simple/etsim.R"), source("models/simple/etsim-ipfp.R"), times = 3)

microbenchmark(source("models/small-area/etsim.R"), source("models/small-area/etsim-ipfp.R"), times =3)

microbenchmark(source("models/sheffield/etsim.R"), source("models/sheffield/etsim-ipfp.R"), times =1)
 
# test the optimised version of ipfp
microbenchmark(source("models/sheffield/etsim-ipfp-optimised.R"), source("models/sheffield/etsim-ipfp.R"), times =2)
