#' Strictness Levels for Objective Correctness Checking
#' @param x object to for objective correctness checking.
#' @param s object to check x against, i.e. s is the candidate solution.
#' @return dumb pasted info at this point
#' @examples
#' \dontrun{
#' strictness(1, 1)
#' }
correctTests <- function(x, s) {

  n <- length(correct_levels())

  # Names of tests
  testname <- c('Identical', 'Equal', 'Equal w. transpose',
                'Equal but wrong type' ,'Same Type & Dimension',
                'Same Type')

  # Initialize score
  score <- rep(NA, n)

  ##################### THIS NEEDS TO BE APPROVED BY JAY
  i <- 1
  while (i <= n) {
    score[i] <- tryCatch({
      correct_levels()[[i]](x, s)
    }, error = function(e){     # Error then score[i] = FALSE
      print(FALSE)
    }, finally = function(f){   # No error then score[i] = TRUE
      print(TRUE)
    })
    if (score[i] == TRUE) {
      i <- i + 1
      break
    } else {
      i <- i + 1
    }
  }
  # Set rest of the checks to TRUE
  if (i <= n) {score[i:n] <- TRUE}
  score[n + 1] <- (sum(score)/n) * 100
  score <- round(score, 3)
  names(score) <- c(testname, 'Total')
  ##################### THIS NEEDS TO BE APPROVED BY JAY

  # For each test that instructor wants to test
  # for (i in 1:n) {
  #   score[i] <- tryCatch({
  #     correct_levels()[[i]](x, s)
  #   }, error = function(e){     # Error then score[i] = FALSE
  #     print(FALSE)
  #   }, finally = function(f){   # No error then score[i] = TRUE
  #     print(TRUE)
  #   })
  # }
  # score[n + 1] <- (sum(score)/n) * 100
  # score <- round(score, 3)
  # names(score) <- c(testname, 'Total')
  return(score)
}

#' Strictness Levels for Objective Correctness Checking
#' @importFrom testthat test_that
#' @importFrom testthat expect_identical
#' @importFrom testthat expect_equal
#' @importFrom testthat expect_type
#' @importFrom methods as
#' @return Dumb pasted info at this point
correct_levels <- function() {

  # Identical Test
  strict1 <- function(x, s) {
    test1 <- testthat::test_that('Identical Checker', {
      testthat::expect_identical(x, s)
    })
    return(test1)
  }

  # Equal Test
  strict2 <- function(x, s) {
    test2 <- testthat::test_that('Equal Checker', {
      testthat::expect_equal(x, s)
    })
    return(test2)
  }

  # Equal Test With Transpose
  strict3 <- function(x, s) {
    lev3 <- test_that('Equal w. transpose', {
      expect_equal(t(x), s)
    })
    return(lev3)
  }

  # Equal Test with type change
  strict4 <- function(x, s) {
    lev4 <- test_that('Equal [with as.type(s)]', {
      expect_equal(as(as.matrix(x), typeof(s)), s)
    })
    return(lev4)
  }

  # Type and Dimension
  strict5 <- function(x, s) {
    lev5 <- test_that('Type & Dimension', {
      expect_equal(dim(x), dim(s))
      expect_type(x, typeof(s))
    })
    return(lev5)
  }

  # Type
  strict6 <- function(x, s) {
    lev6 <- test_that('Type', {
      expect_type(x, typeof(s))
    })
    return(lev6)
  }

  # Group test_that functions
  test <- list(strict1, strict2, strict3, strict4, strict5, strict6)
  return(test)
}

# Notes

# What is commented out is my proposal for setup of strictness levels and tests
# Allow for additional objective test?
# Allow for weights on the tests?

# I'm treating the tests as linear with the while function, need to think about
# that better. See below for a rough idea

# Idea 1:
# We have three main lists of tests: Identical, Equal and other
# Each main list has the same multiple sub tests, which try transposing, changing type and so on...
# When we get a hit in one of the subtests we disregard the rest and calculate the score.

