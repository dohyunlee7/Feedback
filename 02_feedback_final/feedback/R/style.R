#' Check the Style of Submitted Code
#'
#' @details Just a wrapper so check everything in a directory, although
#' we should think about collection of results.
#' @param dirname path to a directory with submitted solution scripts (R
#' and not Rmd at the moment)
#' @return Dumb 10 at this point
#' @examples
#' styleCheck_directory("Hello")
#' @export
styleCheck_directory <- function(dirname) {
  return( 10 )
}

#' Check the Style of Submitted Code
#'
#' @details Wait a sec, my foo.R example in the tests has bad indentation?
#' Apparently `lintr` isn't detecting indentation, while `styler` and
#' `formatR` both do (though clunkier to work with).  The algebra here right
#' now is just placeholder work showing the starting point for the objects
#' to be studied/evaluated.  Need to think about collection of results
#' for various parts of this project, actually.  Because `lintr` only seems
#' to work on a file, I don't envision a style checking on an objects, so if
#' we have an object it would require creating a temp file and then using
#' this function.  Feels a little backwards, but hey.
#' @param filename the name of a file with a path
#' @param verbose debugging temporary option?
#' @importFrom lintr lint
#' @importFrom styler style_text
#' @importFrom formatR tidy_source
#' @export
styleCheck_file <- function(filename, verbose = FALSE) {

  # Future "todo": Allow options/customization of the style rules that
  # will be used/enforced...

  # The final two approaches work from objects so we'll get the file now
  # before we write comments to it:
  z <- readLines(filename)

  # Draft of work via lintr to get a feeling for it
  # Example: filename <- 'tests/testthat/foo.R'
  temp1 <- lintr::lint(filename)
  ans1 <- ""
  res1 <- list()
  for (i in 1:length(temp1)) {
    x <- temp1[[i]]
    if (!(x$linter %in% c("trailing_blank_lines_linter"))) {
      if (is.null(res1[[x$linter]])) {
        res1[[x$linter]] <- 1
      } else {
        res1[[x$linter]] <- res1[[x$linter]] + 1
      }
      ans1 <- c(ans1, "",
                paste0("# lintr flag ", i),
                paste0("# Line number ", x$line_number,
                       ", column number ", x$column_number, ":"),
                paste0("# ", x$line),
                paste0("# ", paste(rep(" ", x$column_number - 1), collapse = ""), "^"),
                paste0("# ", x$message),
                "")
    }
  }


  # Work via formatR
  temp2 <- formatR::tidy_source(text = z,
                                indent = 2, arrow = TRUE, output = FALSE)
  temp2 <- temp2$text.tidy
  temp2 <- unlist(strsplit(temp2, "\n"))   # Argh... why necessary!?



  # Work via styler
  temp3 <- as.vector(styler::style_text(z))



  temp <- temp2       # Or temp3, not sure it will really matter?
  # Goal: indentation, not covered by lintr, ignoring other space
  # fixes, that lintr does pick up on?

  z.ns <- gsub(" |\t|=|<-", "", z)
  temp.ns <- gsub(" |\t|=|<-", "", temp)
  for (i in 1:length(z.ns)) {
    hits <- which(temp.ns == z.ns[i])
    print(hits)
    if (length(hits) == 1) {
      cat("ORIGINAL:", z[i], "\n", sep="")
      cat("styler:::", temp[hits], "\n", sep="")
      z.leading <- gsub("^( *)[^ \t].*$", "\\1", z[i])
      temp.leading <- gsub("^( *)[^ \t].*$", "\\1", temp[hits])
      print(z.leading)
      print(temp.leading)
      if (z.leading != temp.leading) {
        cat("INDENTATION PROBLEM!\n\n")
      } else cat("\n\n")
    }
  }

  # No, above I need to step through because things like } will
  # be important when there are nested closures.  Step through,
  # only looking at "future code" and then only using the first
  # hit that is detected.  Maybe.


  if (verbose) {
    cat("Raw length:", length(z), "\n")
    cat("Raw:\n")
    print(z)
    cat("1. lintr points identified:", sum(unlist(res1)), "\n")
    cat("2. formatR length", length(temp2), "and result:\n")
    print(temp2)
    cat("3. styler length", length(temp3), "and result:\n")
    print(temp3)
  }

  return( length(temp1) *
            ( 1 +
                abs( (length(temp2) - length(x)) /
                       min(c(length(temp2), length(x))) ) +
                abs( (length(temp3) - length(x)) /
                       min(c(length(temp2), length(x))) )
            )
  )

}

## This one might eventually need to write a temporary
## file and lint it (then delete...), I'm not sure.
#styleCheck_object <- function(x) {
#  return( 10 )
#}


#' Provide 80-character width feedback
#'
#' @param submissionfolder a character string describing the directory of scripts
#' to be checked
#' @return Need to define
#' @export
style.80char <- function(submissionfolder) {

  files <- dir(submissionfolder)#, full.names = TRUE, pattern = "\\.[rR]$")

  ans <- rep(TRUE, length(files))
  for (i in 1:length(files)) {
    temp <- dir(paste0(submissionfolder, "/", files[i]))
    temp <- dir(files)[i]
    x <- scan(files[i], what = "", sep = "\n",
              blank.lines.skip = FALSE)
    if (any(nchar(x) > 80)) ans[i] <- FALSE
  }

  # Eventually consider numeric scores instead of logical
  names(ans) <- dir(submissionfolder, pattern = "\\.[rR]$")
  return(list(domain = "style",
              subdomain = "80char",
              score = ans,
              text = NA))

}

#' Provide comment feedback
#'
#' @param submissionfolder a character string describing the directory of scripts
#' to be checked
#' @return Need to define
#' @export
style.comment <- function(submissionfolder) {

  files <- dir(submissionfolder, full.names = TRUE, pattern = "\\.[rR]$")

  ans <- rep(TRUE, length(files))
  for (i in 1:length(files)) {
    x <- scan(files[i], what = "", sep = "\n", blank.lines.skip = FALSE)
    if (!any(grep("# (?=[A-Z])", x, perl=T))) ans[i] <- FALSE
  }

  # Eventually consider numeric scores instead of logical
  names(ans) <- dir(submissionfolder, pattern = "\\.[rR]$")
  return(list(domain = "style",
              subdomain = "comment",
              score = ans,
              text = NA))

}
