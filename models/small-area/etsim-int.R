############################################
#### Integerisation - its impact on model fit
############################################
source("etsim.R")
head(wf[,,4,2,1])


a.v <- as.vector(as.matrix(all.msim)) 
g.v <- as.vector(as.matrix(indf[,,4,3]))
cor(a.v, g.v) # baseline result

# Deterministic integerisation:
intp <- floor(wf[,,4,3]) # truncated weights (1.9 becomes 1)
dr <- wf[,,4,3] - intp # Decimal weights
ints <- as.list(1:nrow(all.msim)) # Rownames
intcat <- ints
intagg <- all.msim * 0 # Aggregate area stats
pops <- all.msim[,1:2]

for (i in 1:nrow(all.msim)){
  if(max(intp[,i]) ==0) intp[which.max(wf[,,4,3][,i]),i] <- 1
  index <- cbind((which(intp[,i]>0)) # generates index
                 ,intp[which(intp[,i]>0),i]) # integers)
  ints[[i]] <- index[rep(1:nrow(index),index[,2])] #clone
  pops$pints[i] <-  length(ints[[i]]) # save integer pops
}


for (i in 1:nrow(all.msim)){
  # while loop to iterate until the population is large enough
  wv <- 1
  while (length(ints[[i]]) < sum(all.msim[i,])){
    wv <- wv - 0.001
    ints[[i]] <- c(ints[[i]], which(dr[,i] < wv & 
                                      dr[,i] >= wv - 0.001))
  }
  pops$pthresh[i] <-  length(ints[[i]])
  pops$thresh[i] <- wv
  #   source("area.cat.R")
  #intcat[[i]] <- area.cat takes up huge amounts of ram - commented out for now
  ############################################
  #### Convert weights back into aggregate
  #### values for each zone
  ############################################
  intagg[i,]   <- colSums(ind.cat[ints[[i]],])
}

head(intagg)
g.v <- as.vector(as.matrix(intagg))
cor(a.v, g.v)



