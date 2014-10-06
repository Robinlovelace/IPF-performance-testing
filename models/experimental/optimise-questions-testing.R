# Question for stack overflow: http://stackoverflow.com/questions/25280117/how-to-rapidly-sample-from-groups-in-r

set.seed(40)
x <- data.frame(matrix(round(runif(1000)), ncol = 10))
x_unique <- x[!duplicated(x),]

# I need to sample each unique row a given number of times so I create a new variable that is simply a concatenation of the variables for each row:

# Another way of seeing x is as a single value - will be useful later
x_code <- do.call(paste0, x)
u_code <- x_code[!duplicated(x)]

# We need a repeated sample sample from x, replicating each unique row s times. This information is provided in the vector s:

s <- rpois(n = nrow(x_unique), lambda = 0.9)

# The question is, how to sample individuals from x to reach the quota set by s, for each unique row? Here's a long and unbeautiful way:

original_method <- function(){

sel <- vector(length = 0)

# for(i in 1:length(s)){
#  sel <- c(sel, sample(which(x_code %in% u_code[i]), size = s[i], replace = T))
# }

sel <- vector(length = 0)

# alternative method that deals with length = 1:
for(i in 1:length(s)){
 xs <- which(x_code %in% u_code[i])
 sel <- c(sel, xs[sample(length(xs), size = s[i], replace = T)])
}

x_sampled <- x[sel, ]
}

# Showing why rep() solution is not OK
unis <- which(u_code %in% x_code[duplicated(x_code)])
s[unis]
unis
u_code[32]
x_code[x_code %in% u_code[32]]
which(x_code %in% u_code[32])

u_code[32] == x_code[32]
u_code[32] == x_code[84]
x[84,] == x_unique[32,]

idx <- rep(1:length(s), times=s)
x_unique[idx, ]

# Do it the dplyr way ###########################
# From here 
library(plyr)
set.seed(1)
dat <- data.frame(id = 1:nrow(x), Category = x_code)

sampleOne <- function(id, fraction=0.9){
  sort(sample(id, round(length(id) * fraction)))
}

ddply(dat, .(Category), summarize, sampleID = sample(id, size = 2, replace = T))

# the Map and Wickham way:
MapMethod <- function(){
set.seed(40)
index <- 1:nrow(x)
grouped_index <- split(index, x, drop = TRUE)
names(grouped_index) <- NULL

sample2 <- function(x, n, ...) {
  if (length(x) == 1) return(rep(x, n))
  sample(x, n, ...)
}

sel <- unlist(Map(sample2, grouped_index, s, replace = TRUE))
sel <- sort(sel)

x_sampled_wickham <- x[sel, ]}
x_sampled == x_sampled_wickham

microbenchmark(MapMethod(), original_method())
