#'
#' # Special header from The Professor!
#' 
#' Whatever instructions are wanted could be here, separate from
#' the individual problem below.  Here, in this document, Jay is proposing
#' rules to support the current challenge of "split-then-rejoin-later".


### ProblemStartsHere ###

#' 
#' # Problem 1
#' 

NR <- 4
NC <- 100000
x <- matrix(sample(1:10, NR*NC, replace = TRUE), NR, NC)

### Challenge: calculate the column means; save the answer
### in a vector of length 'NC' called 'ans'.  

#'
#' ### Your work below
#' 
#' When this script is run, it should have created a vector of length
#' `NC` called `ans`.  Obviously we expect it to be numeric and have
#' values that are often close to 5.5 plus or minus a little given
#' what we know about probability and sampling.
#' 

ans <- c()
for (j in 1:NC) {
  temp <- 0
  for (i in 1:NR)
    temp <- temp + x[i,j]
  temp <- temp / NR
  ans[j] <- temp
}

### ProblemEndsHere ###

### ProblemStartsHere ###

#' 
#' # Question 2
#' 

### Challenge: Write the body of the function, below, which
### seeks to return the data frame with one type of modification:
### Every numeric (but not integer) column should be standardized
### to have mean 0 and variance 1.


myfunc <- function(x) {
sd <- sd(x)
mean <- mean(x)
# Start your work here
for (i in 1:length(x)){
  if(all.equal(x[i], as.integer(x[i]))){
  x[i] <- x[i]
}
    else{x[i] <- (x[i] - mean)/sd
    }
  }
  # End your work here
  
  return(x)  
}

# Example for your testing (if you want).  It should modify the
# first four columns but leave the last column unchanged.

ans <- myfunc(iris)

summary(iris)
summary(ans)

### ProblemEndsHere ###
