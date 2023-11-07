#' @title Combine each question script with grades/feedback and generate total
#' grades.csv
#' @param pset.dir directory path (scratch folder)
#' @description Combine each question script, with question level grade and
#' feedback in "combined_submissions/". Also appends total grade at bottom of
#' script and creates the total grades.csv
#' @example \dontrun {
#' combine.grading(dir = "scratch_folder")
#' }
#' @export
combine.grading <- function(pset.dir) {
  # Create folder for combined submissions with grades
  dir.create("combined_submissions", showWarnings = FALSE)
  prob_num <- dir()[grep("submissions-Q", dir())]
  
  # Append scores at end of each question (not entire pset)
  for (i in 1:length(prob_num)) {
    temp_grades <- read.csv(paste0(prob_num[i], "/grades-Q", i, "/grades-Q", i,
                                   ".csv"))
    row.names(temp_grades) <- temp_grades[, 1]
    temp_grades <- temp_grades[, -1]
    
    sub <- file.path((paste0(prob_num[i], "/", prob_num[i])),
                     dir(paste0(prob_num[i], "/", prob_num[i])))
    
    for (j in 1:length(sub)) {
      write(paste0("#\tStyle Score for Problem ", i, ": ",
                   temp_grades[j, "style"]), file = sub[j], append = TRUE)
      write(paste0("#\tCorrectness Score for Problem ", i, ": ",
                   temp_grades[j, "correctness"]), file = sub[j], append = TRUE)
    }
  }
  
  # Combine all questions (with question specific feedback/grades) into one file
  sub <- list()
  for (i in 1:length(prob_num)) {
    sub[[i]] <- file.path((paste0(prob_num[i], "/", prob_num[i])),
                          dir(paste0(prob_num[i], "/", prob_num[i])))
  }
  # Should we use "i" instead of "1"? Is is possible to have more submissions
  # for question 1 than question 2?
  for (j in 1:length(sub[[1]])) {  # Number of student submissions
    x <- list()
    for (k in 1:length(prob_num)) {
      x[[k]] <- scan(sub[[k]][j], what = "", sep = "\n")
    }
    writeLines(text = unlist(x),
               con = paste0("combined_submissions/", dir("submissions")[j]))
  }
  
  # Create total grades
  prob_num <- dir()[grep("submissions-Q", dir())]
  temp_grades <- list()
  for (i in 1:length(prob_num)) {
    temp_grades[[i]] <- read.csv(paste0(prob_num[i], "/grades-Q", i,
                                        "/grades-Q", i, ".csv"))
    row.names(temp_grades[[i]]) <- temp_grades[[i]][, 1]
    temp_grades[[i]] <- temp_grades[[i]][, -1]
  }
  corr <- rep(NA, nrow(temp_grades[[1]]))
  sty <- rep(NA, nrow(temp_grades[[1]]))
  # Add algorithmic similarity score here?
  
  for (j in 1:nrow(temp_grades[[1]])) {
    x <- rep(NA, length(temp_grades))
    y <- rep(NA, length(temp_grades))
    for (k in 1:length(temp_grades)) {
      x[k] <- temp_grades[[k]]$correctness[j]
      y[k] <- temp_grades[[k]]$style[j]
      # Add agorithimc similarity score here?
    }
    corr[j] <- sum(x)
    sty[j] <- sum(y)
    # Algo score here?
  }
  stu <- dir("submissions")
  stu <- gsub(".R$", "", stu)
  total_grades <- data.frame(correctness = corr, style = sty, row.names = stu)
  write.csv(total_grades, file = "grades/grades.csv")
  
  # Append total grades to bottom of each combined script
  for (m in 1:length(stu)) {
    write(paste0("#\n"),
          file = paste0("combined_submissions/", stu[m], ".R"), append = TRUE)
    write(paste0("#\tTotal Grades:"),
          file = paste0("combined_submissions/", stu[m], ".R"), append = TRUE)
    write(paste0("#\t\tStyle Grade: ", total_grades[stu[m], "style"]),
          file = paste0("combined_submissions/", stu[m], ".R"), append = TRUE)
    write(paste0("#\t\tCorrectness Grade: ", total_grades[stu[m], "correctness"]),
          file = paste0("combined_submissions/", stu[m], ".R"), append = TRUE)
    # write(paste0("#\t\Algorithmic Grade: ", total_grades[stu[m], "similarity"]),
    #       file = paste0("combined_submissions/", stu[m], ".R"), append = TRUE)
  }
}
