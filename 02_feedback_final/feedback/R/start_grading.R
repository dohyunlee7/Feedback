#' @title Function that splits solution and submissions into question-level
#' folders. It does the style, correctness and similarity checks using
#' `stylemethod.R`, `correctmethod.R`, and `algomethod.R`, generating the
#' corresponding scores.
#' @param dir directory path for a new problem set (scratch folder)
#' @description This function incorporates splitting the problem set and grading
#' it. It splits the mastersolution.R and all submissions given in `dir` into
#' question-level folders (ie. grades-Q1/, sol_dir-Q1/, submission-Q1/), and
#' places the split scripts into their correspoding folders. It does style check
#' and correctness check at the question level. For style check, it also appends
#' written feedback (not scores) at the end of question-level submissions. It
#' then records the grades in question-level gradebook CSVs. After this is done,
#' questions can be passed off to TFs for the algo work.
#' @example \dontrun {start.grading("scratch_folder")}
#' @export
start_grading <- function(dir) {
  # Style linters to check as inputted by instructor. Returns a data frame with
  # names(linter_names) either being "linter_names" or "tags", depending input
  linter_names <- style_input()

  # Consider running style_check_dir() on all submissions here. This could be
  # used to incorporate "consistency" in style score

  # Split solution directory and submissions directory
  sol <- "sol_dir"
  sub_fol <- "submissions"
  split_script(sol, sub_fol)

  # Start grading for style and correctness
  # NOTE: style is checked on a directory, while correctness is on a single file
  prob_num <- dir()[grep("submissions-Q", dir())]
  # Style score placeholder
  sty <- vector(mode = "list", length = length(prob_num))
  # Data frame of style errors
  df_count <- vector(mode = "list", length = length(prob_num)) #

  for (i in 1:length(prob_num)) {
    # Correctness starts here

    # Identify solution file and student submissions
    # NOTE: This takes the first file in sol_dir-Q as the mastersolution. Maybe
    # re-think this along with split_script()
    sol <- paste0(prob_num[i], "/sol_dir-Q", i, "/",
                  dir(paste0(prob_num[i], "/sol_dir-Q", i, "/"))[1])
    sub <- file.path((paste0(prob_num[i], "/", prob_num[i])),
                     dir(paste0(prob_num[i], "/", prob_num[i])))

    # Correctness and style score
    corr <- rep(NA, length(sub))

    # Style check on question level directory
    directory <- paste0(prob_num[i], "/", prob_num[i])
    df_count[[i]] <- style_check_dir(directory, linter_names)
    row.names(df_count[[i]]) <- dir(directory)
    sty[[i]] <- assign_grade(df_count[[i]])

    for (j in 1:length(sub)) {
      # Objective correctness on individual submissions (not a directory)
      print(paste("Processing ", sub[j]))
      corr[j] <- correctCheck_file(sub[j], sol)$correctCheck["Total"]


      # Append specific written style feedback to each submission (not a score)
      # This is done on individual scripts. Consider moving this work into
      # style_check_dir(), then style_feedback() would no longer be needed
      sty_feedback <- style_feedback(sub[j], linter = linter_names)

      # NOTE: Change the implementation so the result of style_feedback() is
      # written to a csv, then later during combine.grading() that csv is used
      # to cbind() written feedback during the combine. That way if style is
      # checked more than once, you don't get multiple feedbacks appended to a
      # script.
      if (nrow(sty_feedback) > 0) {
        write(paste0("#\tStyle Feedback for Problem ", i, ":"),
              file = sub[j], append = TRUE)
        write(paste0("#\t(Only the first instance of each type of style error is displayed below):"),
              file = sub[j], append = TRUE)
        for (k in 1:nrow(sty_feedback)) {
          write(paste0("#\t\tTotal Instances: ", as.data.frame(df_count[[i]][j, which(df_count[[i]][j, ] > 0)])[1, k],
                       "\t\tLine number: ",  sty_feedback[k,1],
                       "\t\tMessage: ", sty_feedback[k,2]),
                file = sub[j], append = TRUE)
        }
      }
    }
    # NOTE: Algorithmic similarity not performed yet. To be done later by TFs?

    # NOTE: df_count can be used to incorporate "consistency" in calculating
    # style score. (ie. Did a student have more instances of the same type of
    # mistake in question 2, as opposed to in question 1? Or less? Need to
    # figure out how to incorporate "consistency" into final style score.

    # Write grades to question-level gradebook
    sub <- gsub(".*/", "", sub)
    temp <- data.frame(correctness = corr, style = unlist(sty[[i]]),
                       row.names = sub)
    write.csv(temp,
              file = paste0(prob_num[i], "/grades-Q", i, "/grades-Q", i, ".csv")
              )
  }
}
