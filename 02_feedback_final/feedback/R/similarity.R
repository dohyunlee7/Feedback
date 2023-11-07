#' Algoritmic similarity check for both directories
#' @param student_dir Object to for objective correctness checking.
#' @param solution_dir Object to check x against, i.e. s is the candidate solution.
#' @importFrom SimilaR SimilaR_fromTwoFunctions
#' @return Dumb pasted info at this point
#' @examples
#' \dontrun{
#' print("Hello")
#' }
algoCheck_all <- function(student_dir, solution_dir) {
  # Get names of student and solution libraries
  x_names <- dir(student_dir, full.names = TRUE) # Submissions
  s_names <- dir(solution_dir, full.names = TRUE) # Solutions
  all <- c(x_names, s_names)
  n <- length(all)

  # Initialize empty list of length of student directory
  x <- vector("list", n)
  for (i in 1:n){
    x[[i]] <- parse_algo(all[i])
  }

  # Initialize matrix
  D <- matrix(NA, n, n)
  # For-loop
  for (i in 1:n) {
    for (j in 1:n) {
      # Get Similarity
      ans <- SimilaR::SimilaR_fromTwoFunctions(x[[i]], x[[j]])$SimilaR
      # Add similarity score to matrix
      D[i, j] <- round(ans, 2)
    }
  }
  # If symmetrical then set the diagonals as 0
  # if (isSymmetric(D)) {diag(D) <- 0}
  # Return distance
  return(D)
}

#' Similarity Check for two libraries
#' @param student_dir Object to for objective correctness checking.
#' @param solution_dir Object to check x against, i.e. s is the candidate solution.
#' @importFrom SimilaR SimilaR_fromTwoFunctions
#' @return Dumb pasted info at this point
#' @examples
#' \dontrun{
#' algoCheck_directory(1, 1, c(0.5, 0.5))
#' }
#' @export
algoCheck_directory <- function(student_dir, solution_dir) {
  # Get names of student and solution libraries
  x_names <- dir(student_dir, full.names = TRUE) # Submissions
  s_names <- dir(solution_dir, full.names = TRUE) # Solutions

  n <- length(x_names)
  m <- length(s_names)

  # Initialize empty list of length of student directory
  x <- vector("list", n)
  for (i in 1:n){
    x[[i]] <- parse_algo(x_names[i])
  }

  # Initialize empty list of length of solution directory
  s <- vector("list", m)
  for (j in 1:m){
    s[[j]] <- parse_algo(s_names[j])
  }

  # Initialize matrix
  D <- matrix(NA, n, m)
  # For-loop
  for (i in 1:n) {
    for (j in 1:m) {
      # Get Similarity
      ans <- SimilaR::SimilaR_fromTwoFunctions(x[[i]], s[[j]])$SimilaR
      # Add similarity score to matrix
      D[i, j] <- round(ans, 2)
    }
  }
  # If symmetrical then set the diagonals as 0
  # if (isSymmetric(D)) {diag(D) <- 0}
  # Return distance
  return(D)
}

#' Algorithm of an R File
#' @param filename path to a R script (R and not Rmd at the moment)
#' @details ...
#' @examples
#' \dontrun{
#' parse_algo('Solution.R')
#' }
parse_algo <- function(filename) {
  # Create a function wrapper around a script
  mytxt <- c("function() {", "set.seed(1)", readLines(filename), "}")
  # Set all set.seeds at set.seed(1)
  mytxt <- gsub("set.seed\\(*\\d*\\)", "set.seed\\(1\\)", mytxt)
  # Create a function
  myfunc <- eval(parse(text = mytxt))
  return(myfunc)
}

# Notes:

# I have solution_dir and student_dir, but we use this also to check the
# similarity between studentA vs. studentB. Thus, we maybe want to use dir1 and
# dir2 as params instead of solution_dir and student_dir?
