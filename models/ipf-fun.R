### custom ipf code - from gregor (2008

#inputs:
#1) margins_ - a list of margin values with each component equal to a margin
#Example: row margins of 210 and 300, column margins of 140 and 370
#and third dimension margins of 170 and 340.  
#[[1]]     [,1] [,2] [,3]
#210, 300
#[[2]]
#140, 370
#[[3]]
#170, 340
#2) seedAry - a multi-dimensional array used as the seed for the IPF
#3) iteration counter (default to 100)
#4) closure criteria (default to 0.001)

#For more info on IPF see:
#Beckmann, R., Baggerly, K. and McKay, M. (1996). "Creating Synthetic
# Baseline Populations."
# #   Transportation Research 30A(6), 415-435.
# #Inro. (1996). "Algorithms". EMME/2 User's Manual. Section 6.
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~

ipf <- function(Margins_, seedAry, maxiter=100, closure=0.001) {
    #Check to see if the sum of each margin is equal
    MarginSums. <- unlist(lapply(Margins_, sum))
    if(any(MarginSums. != MarginSums.[1])) warning("sum of each margin
not equal")

    #Replace margin values of zero with 0.001
    Margins_ <- lapply(Margins_, function(x) {
          if(any(x == 0)) warning("zeros in marginsMtx replaced with
0.001") 
          x[x == 0] <- 0.001
          x
          })

    #Check to see if number of dimensions in seed array equals thenumber of
    #margins specified in the marginsMtx
    numMargins <- length(dim(seedAry))
    if(length(Margins_) != numMargins) {
        stop("number of margins in marginsMtx not equal to number of margins in seedAry")
    }

    #Set initial values
    resultAry <- seedAry
    iter <- 0
    marginChecks <- rep(1, numMargins)
    margins <- seq(1, numMargins)

    #Iteratively proportion margins until closure or iteration criteria are met
    while((any(marginChecks > closure)) & (iter < maxiter)) {
        for(margin in margins) {
            marginTotal <- apply(resultAry, margin, sum)
            marginCoeff <- Margins_[[margin]]/marginTotal
            marginCoeff[is.infinite(marginCoeff)] <- 0
            resultAry <- sweep(resultAry, margin, marginCoeff, "*")
            marginChecks[margin] <- sum(abs(1 - marginCoeff))
        }    
        iter <- iter + 1
    }
    
    #If IPF stopped due to number of iterations then output info
    if(iter == maxiter) cat("IPF stopped due to number of iterations\n")

    #Return balanced array
    resultAry
}


