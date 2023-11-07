#' @title Splits solution file and all submissions into question level folders
#' @param sol_dir directory path containing the mastersolution.R
#' @param sub_dir directory path containing all submissions
#' @description function splits solution and submission scripts (using specific
#' tags within scripts (ie. ### ProblemStartsHere ###, ### ProblemEndsHere ###).
#' It then creates a directory within the scratch_folder for each question,
#' (ie. grades-Q1/, sol_dir-Q1/, submission-Q1/).
#' @example \dontrun {
#' split_script(sol_dir = "scratch_folder/sol_dir/,
#' sub_dir = "scratch_folder/submissions/"
#' }
#' @export
split_script <- function(sol_dir, sub_dir){

  #if (!file.exists(solutionfile)) stop("Solution file does not exist!")
  # Check that solution and submissions directories exist and are not empty
  if (!dir.exists(sol_dir)) stop("Solutions directory does not exist!")
  if (!dir.exists(sub_dir)) stop("Submissions directory does not exist!")
  sol_files <- file.path(sol_dir, dir(sol_dir))
  if (length(sol_files) == 0) {
    stop("There are no solutions in solutions directory.")
  }
  sub_files <- file.path(sub_dir, dir(sub_dir))
  if (length(sub_files) == 0) {
    stop("There are no student submissions in submissions directory.")
  }

  # Split the solutions files here. Primary solution file should be called
  # mastersolution.R
  x <- lapply(sol_files, scan, what = "", sep = "\n")
  names(x) <- gsub(".*/", "", sol_files)
  # Get start and end line for each question in all solution files
  x_slines <- vector(mode = "list", length = length(x))
  names(x_slines) <- names(x)
  x_elines <- vector(mode = "list", length = length(x))
  names(x_elines) <- names(x)

  # Find start/end of problems in solution files
  for (i in 1:length(x)) {
    x_slines[[i]] <- grep("ProblemStartsHere", x[[i]])
    x_elines[[i]] <- grep("ProblemEndsHere", x[[i]])
    if (length(x_slines[[i]]) != length(x_elines[[i]]) |
        length(x_slines[[i]]) == 0) {
      stop(paste0("Start/end problem in file ", names(x)[i]))
    }
    if (x_slines[[i]][1] >= x_elines[[i]][1]) {
      stop(paste0("First type of label problem in file ", names(x)[i]))
    }
    if (any(x_slines[[i]][-1] - x_elines[[i]][-length(x_elines[[i]])] <= 0)) {
      stop(paste0("Nested labels in file ", names(x)[i]))
    }
  }

  # The number of questions is only based on number of questions in
  # mastersolution.R
  numQ <- length(x_slines$"mastersolution.R")
  # Check all solution files have same number of questions as mastersolution.R
  temp <- mapply(length, x_slines)
  if(!all(temp == numQ)) {
    stop(paste0("Number of questions in (", names(x_slines)[which(temp != numQ)],
    ") do not equal the number of questions in solution.R"))
  }

  # Check if questions in solution files are labelled correctly
  for (i in 1:length(x_slines)) {
    if (length(x_slines[[i]]) != length(x_elines[[i]]) | length(x_slines) == 0) {
      stop(paste0("The start/end of problems in (", names(x_slines)[i],
                  ") solution file not labelled correctly."))
    }
    if (x_slines[[i]][1] >= x_elines[[i]][1]) {
      stop(paste0("The start/end of problems in (", names(x_slines)[i],
                  ") solution file not labelled correctly."))
    }
    if (any(x_slines[[i]][-1] - x_elines[[i]][-length(x_elines[[i]])] <= 0)) {
      stop("The start/end of problems in (", names(x_slines)[i],
           ") solution file not labelled correctly. Possibly nested loops.")
    }
  }

  # Create question specific folders: submissions-Q/, sol_dir/ and grades/
  for (i in 1:numQ) {
    dir.create(paste0(sub_dir,"-Q", i), showWarnings = FALSE)
    temp <- gsub(".*/", "", sub_dir)
    dir.create(paste0(sub_dir, "-Q", i, "/", temp, "-Q", i),
               showWarnings = FALSE)
    dir.create(paste0(sub_dir, "-Q", i, "/sol_dir-Q", i),
               showWarnings = FALSE)
    dir.create(paste0(sub_dir, "-Q", i, "/grades-Q", i),
               showWarnings = FALSE)
  }

  # Student submissions starts here
  z <- lapply(sub_files, scan, what = "", sep = "\n")
  names(z) <- gsub(".*/", "", sub_files)

  # Find start/end of problems in student submissions
  z_slines <- vector(mode = "list", length = length(z))
  z_elines <- vector(mode = "list", length = length(z))

  for (i in 1:length(z)) {
    z_slines[[i]] <- grep("ProblemStartsHere", z[[i]])
    z_elines[[i]] <- grep("ProblemEndsHere", z[[i]])
    if (length(z_slines[[i]]) != length(z_elines[[i]]) |
        length(z_slines[[i]]) == 0) {
      stop(paste0("Start/end problem in file ", names(z)[i]))
    }
    if (z_slines[[i]][1] >= z_elines[[i]][1]) {
      stop(paste0("First type of label problem in file ", names(z)[i]))
    }
    if (any(z_slines[[i]][-1] - z_elines[[i]][-length(z_elines[[i]])] <= 0)) {
      stop(paste0("Nested labels in file ", names(z)[i]))
    }
  }

  # Add question specific solution(s) into question folder's sol_dir
  for (i in 1:length(x)) {
    for (j in 1:numQ){
      writeLines(x[[i]][x_slines[[i]][j]:x_elines[[i]][j]],
                 paste0(sub_dir, "-Q", j, "/sol_dir-Q", j, "/",
                        gsub(".R", "", names(x)[i]), "-Q", j, ".R"))
    }
  }

  # Add question specific submissions into each question's folder
  for (i in 1:length(z)) {
    for (j in 1:numQ){
      writeLines(z[[i]][z_slines[[i]][j]:z_elines[[i]][j]],
                 paste0(sub_dir, "-Q", j, "/submissions-Q", j, "/",
                        gsub(".R", "", names(z)[i]), "-Q", j, ".R"))
    }
  }
}





