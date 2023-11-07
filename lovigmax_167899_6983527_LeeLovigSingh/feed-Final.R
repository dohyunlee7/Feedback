library(R.utils)

#' @param student A list of functions submitted by your students
#' @param correct A function which is the "correct" solution
#' @param test A list of test inputs which will be used to compare the 
#' submissions
#' @param timeLimit The seconds until we force the code to return "DNR"
#' @param customs A list of custom test functions. These test functions must be 
#' of form function(x,y) where x is a student's output and y is the correct 
#' output. This function should return a boolean.
#' @param errCodes An array of strings where the i-th element is the error code
#' added if the i-th test in customs is failed.
#' @param attr A boolean determining if all.equal() returns attribute errors
#' @return A matrix where each element is a list of errors made
#' @example 
#' student_subs2 <- dir("ExampleSubmissions-2")
#' student_answers2 <- vector("list", length(student_subs2))
#' for (i in 1:length(student_subs2)) {
#'    print(i)
#'    try(source(paste0("ExampleSubmissions-2/", student_subs2[i])))
#'    try(student_answers2[[i]] <- myfunc)
#'    print(myfunc)
#' }
#' customFunc1 <- function(x,y) {
#'    return(all(names(x) == names(y)))
#' }
#'
#' customFunc2 <- function(x,y) {
#'    return(typeof(x) == "character")
#' }
#'
#' errCodes <- c("Diff Names", "Isn't Character")
#'
#' customs <- list(customFunc1, customFunc2)
#'
#' A <- check_equiv(student_answers2[-1], student_answers2[[1]], 
#'                 list(iris, cars), timeLimit = 1, customs,
#'                 errCodes = errCodes)
#'
checkEquiv <- function(student, correct, test, timeLimit = 1,
                       customs = NULL, errCodes = NULL, attr = F) {
  equiv_subs <- c(rep(list(NULL), length(student) * length(test)))
  k <- length(test)
  for (i in 1:length(student)) {
    for (j in 1:length(test)) {
      temp <- NULL
      try(temp <- withTimeout(student[i][[1]](test[[j]]), timeout = timeLimit,
                              onTimeout = "silent"), silent = T)
      ae <- all.equal(correct(test[[j]]) ,temp, check.attributes = attr)
      
      # If list is NULL, assign DNR (Did not run)
      # If list is TRUE, assign an empty list
      # If neither, assign errors from all.equal()
      if (is.null(temp)) {
        equiv_subs[(i - 1) * k + j][[1]] <- c("DNR")
      } else if(is.logical(ae)) {
        equiv_subs[(i - 1) * k + j][[1]] <- list()
      } else {
        equiv_subs[(i - 1) * k + j][[1]] <- ae
      }
      
      # Run test case j through the correct function
      cor <- correct(test[[j]])
      
      # Code used to run custom test functions written by the user
      # First, we check if there are any custom tests and
      # if code didn't run then assign error codes based on input
      if (!is.null(customs))
        for (n in 1:length(customs)) {
          if (!is.null(temp) && !customs[[n]](temp, cor)) {
            if(length(equiv_subs[(i - 1) * k + j][[1]]) == 0) {
              equiv_subs[(i - 1) * k + j][[1]] <- c(errCodes[n])
            } else {
              equiv_subs[(i - 1) * k + j][[1]] <- 
                c(equiv_subs[(i - 1) * k + j][[1]], errCodes[n])
            }
          }
        }
    }
  }
  equiv_subs <- matrix(equiv_subs, nrow = length(student), byrow = T)
  return(equiv_subs)
}


# This is a function which is used in the scoreFuncs function
# This function returns a vector of 0/1's if, ret[i] = 1 if V[i] is in L
# 0 otherwise, small little function we need below
retInds <- function(L, V) {
  ret <- rep(0, length(V))
  for (i in 1:length(V)) {
    if (V[i] %in% L) {
      ret[i] <- 1
    }
  }
  return(ret)
}


#' Return 3 objects relevant to badness
#' 
#' @param mat A n cross m matrix of lists of characters where each row is the 
#' i-th student (i = 1,...,n) and the j-th column is the j-th test case
#' (j = 1,...,m) and mat[i,j] is a list of characters representing the errors by
#' the i-th student on the j-th test case.
#' @param U A vector of the errors that all students made over all test cases
#' @param WErr A vector which element WErr[i] "badness" score on error U[i]
#' @param WCase A vector which element WCase[i] is a weight of importance on 
#' case i
#' @return A list for the "weighted badness" for each students code, the first
#' index is the cumulative badness vector, the 2nd index in the "by-case" 
#' badness, the 3rd index is the binarized error matrix
#' @example 
#' #This is a case with 2 students and two test cases, 
#' #Student 1 has error messages a & b on case 1 and no errors on case 2
#' #Student 2 has error messages c on case 1 and a & b & c on case 2
#' vec <- list(list("a", "b"), list(), list("c"), list("a", "b", "c"))
#' 
#' mat <- matrix(vec, nrow = 2, byrow = T)
#' U <- unique(unlist(mat))
#' WErr <- c(0, 1, 2)
#' WCase <- c(1,1)
#' 
#' # Default Weights of ones vectors (all errors & test cases are equally "bad")
#' scoreFuncs(mat)
#' # Custom Weights
#' scoreFuncs(mat, U, WErr, WCase)
#' 
scoreFuncs <- function(mat, U = NULL, WErr = NULL, WCase = NULL) {
  
  if (is.null(U)) {
    U <- unique(unlist(mat))
  }
  # If user does not put list of weights corresponding to U then it defines
  # each error as equally "bad" except for DNR, which we flag with a 
  # negative score
  if (is.null(WErr)) {
    WErr <- rep(1, length(U))
    WErr[which(U == "DNR")] <- -1
  }
  
  # Same for weights over the cases
  if (is.null(WCase)) {
    WCase <- rep(1, ncol(mat))
  }
  
  # We initialize our binarized list
  binlist <- rep(list(rep(0,length(U))), nrow(mat) * ncol(mat))
  
  # We loop through each element of mat and use function retInds to return the
  # binarized index
  for (x in 1:nrow(mat)) {
    for (y in 1:ncol(mat)) {
      binlist[[ncol(mat) * (x - 1) + y]] <- retInds(mat[x, y][[1]], U)
    }
  }
  
  # We now matricize the binary list
  binmat <- matrix(binlist, nrow = nrow(mat), byrow = T)
  
  scoremat <- matrix(rep(0, nrow(binmat) * ncol(binmat)), nrow = nrow(mat))
  
  for (x in 1:nrow(scoremat)) {
    for (y in 1:ncol(scoremat)) {
      scoremat[x, y] <- sum(binmat[x, y][[1]] * WErr)
    }
  }
  
  score <- scoremat %*% WCase
  
  return(list(score, scoremat, binmat))
}

# This is a function used in getGrade when curved = T
# This function finds the x quantile normal(80,5) value and restricts it
# to be a value in [0,100]
boundNorm <- function(x) {
  q <- qnorm(x, 80, 5)
  q <- max(0, q)
  q <- min(100, q)
  return(q)
}

#' Converting cumulative badness to grades
#' 
#' @param vec The first element in the list produced by scoreFuncs, it is the
#' cumulative badness scores for each submission
#' @param Curved Weather the grades should be on a proportional or curved 
#' (normal(80,5)) scale, by default this False (proportional scale is chosen)
#' 
#' @return A vector where the i-th element is the grade for the i-th submission
#' @example 
#' L <- list(
#'     list("A"), list("A", "B"), list("DNR"), list("DNR"), 
#'     list("C"), list(), list(), list()
#' )
#' A <- matrix(L, nrow = 4, byrow = T)
#' B <- scoreFuncs(A)
#' getGrade(B[[1]], curved = T)
getGrade <- function(vec, curved = F) {
  vec <- as.vector(vec)
  grad <- rep(0, length(vec))
  for (i in 1:length(vec)) {
    if (vec[i] > 0) {
      if (curved) {
        grad[i] <- boundNorm(.999 * (1 - vec[i] / max(vec)) + .5 * .001)
      } else {
        grad[i] <- 100 - (100 * vec[i] / max(vec))
      }
    }
    if (vec[i] == 0) {
      grad[i] <- 100
    }
  }
  return(grad)
}

#' Simple Wrapper function to have assignments graded in 1 line with defaults
#' 
#' @param student A list of functions submitted by your students
#' @param correct A function which is the "correct" solution
#' @param test A list of test inputs which will be used to compare the 
#' submissions
#' @param Curved Weather the grades should be on a proportional or curved 
#' (normal(80,5)) scale, by default is False
#' 
#' @return A vector where the i-th element is the grade for the i-th submission
gradeFunctionsSimple <- function(studentSubs, correctSub, tests, curved = F) {
  step1 <- checkEquiv(studentSubs, correctSub, tests)
  step2 <- scoreFuncs(step1)
  step3 <- getGrade(step2[[1]], curved)
  return(step3)
}

# Full Example

# Data prep
student_subs2 <- dir("ExampleSubmissions-2")

# Creating a list of functions
student_answers2 <- vector("list", length(student_subs2))
for (i in 1:length(student_subs2)) {
  print(i)
  try(source(paste0("ExampleSubmissions-2/", student_subs2[i])))
  try(student_answers2[[i]] <- myfunc)
  print(myfunc)
}

# Examples of custom test functions
customFunc1 <- function(x,y) {
  return (all(names(x) == names(y)))
}

customFunc2 <- function(x,y) {
  return (typeof(x) == "character")
}

errCodes <- c("Diff Names", "Isn't Character")

customs <- list(customFunc1, customFunc2)

# Example where we use custom test cases and time limit
A <- checkEquiv(student_answers2[-1], student_answers2[[1]], list(iris, cars),
                 timeLimit = 1, customs, errCodes = errCodes)

# Example where we use only default inputs
A <- checkEquiv(student_answers2[-1], student_answers2[[1]], list(iris, cars))

# Finding all unique errors, if we wanted to assign custom weight, we need this
UA <- unique(unlist(A))

# Badness scores, B[[1]] is the cumulative badness score, 
# B[[2]] is the by-case scores, B[[3]] is the binarized matrix of errors
B <- scoreFuncs(A)

# Code to find most occurring errors
table(unlist(A))[order(table(unlist(A)))]

# Grades for curved scores vs non-curved scores
hist(getGrade(B[[1]]))
hist(getGrade(B[[1]], curved = T))

# Wrapper function to calculate grades with defaults
grades <- gradeFunctionsSimple(student_answers2[-1], 
                               student_answers2[[1]], 
                               list(iris), 
                               curved = F)

# Histogram of grades alongside matrix of submission names and grades
hist(grades)
matrix(c(student_subs2[-1], round(grades)), ncol = 2)
