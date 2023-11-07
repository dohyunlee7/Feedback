#' @title Function checks style on a directory of scripts (not individual files).
#' @param sub_folder directory path on which to check style
#' @param linter string of linter names or tags used to check style. This is
#' provided by style_input(). The default is to use all linters found in
#' available_linters(). NOTE: Consider creating a csv of desired linters as
#' documented in lint(). Current implementation runs lint() using all linters,
#' and we only select the desired ones.
#' @description function that uses lintr to check the style format of a
#' directory of scripts. `linter` is obtained from instructor by previously
#' using style_input().
#' @return This function returns a data frame containing the number of errors in
#' all scripts for each inputted lintr. These errors will be later used with a
#' weight vector to calculate the final style score.
#' @example \dontrun {
#' style_check_file(sub_folder = "submissions",
#' linter = c("assignment_linter", "commas_linter")
#' }
#' @export
style_check_dir <- function(sub_folder, linter = available_linters()) {
  # Get the list of available linters and tags from lintr
  avail_linters <- available_linters()
  avail_tags <- available_tags()

  # Lints using linter names (not tags)
  if (names(linter) == "linter_names") {
    # If cannot find a linter name, throw an error.
    if(!all(linter$linter_names %in% avail_linters$linter)) {
      stop(paste0(linter[!(linter$linter_names %in% avail_linters$linter), ],
                  " not found."))
    }
    # to_check are linters to check
    to_check <- avail_linters[
      which(avail_linters$linter %in% linter$linter_names), ]$linter
  }
  # Lints using linter tags (ie. "best_practices" or "common_mistakes", etc.")
  if (names(linter) == "tags") {
    # If cannot find a linter tag, throw an error.
    if(!all(linter$tags %in% avail_tags)) {
      stop(paste0(linter[!(linter$tags %in% avail_tags), ],
                  " not found."))
    }
    # Find linter names that are associated with the inputted linter tag
    # First grab linter tags from avail_linters
    temp <- data.frame(matrix(data = NA, nrow = nrow(avail_linters), ncol = 1))
    for (i in 1:nrow(temp)) {
      temp[i,1] <- paste(unlist(avail_linters[i, "tags"]), collapse = ", ")
    }
    # to_check are the linter names associated with the linter tags being used
    to_check <- avail_linters[which(grepl(linter$tags, temp[,1])), "linter"]
  }

  # Running count of all student errors for selected linters. This is used in
  # calculating weightVec for style score
  df_count <- as.data.frame(
    matrix(data = rep(0, length(dir(sub_folder)) * length(to_check)),
           nrow = length(dir(sub_folder)), ncol = length(to_check)))
  names(df_count) <- to_check
  row.names(df_count) <- dir(sub_folder)

  # Run lint on each submission and count number of occurrences for each unique
  # linter type.
  files <- file.path(sub_folder, dir(sub_folder))
  for (i in 1:length(files)) {
    linted <- lint(files[i])
    for(m in linted) {
      if (m$linter %in% to_check) {
        df_count[i, m$linter] <- df_count[i, m$linter] + 1
      }
    }
  }
  # Consider returning a list of df_count and a data frame of written feedback.
  # That would make the style_feedback() a part of this function, and then
  # style_feedback() would no longer be needed.
  return(df_count)
}

#' @title Function checks style of individual files (not a directory of files).
#' @param filename script path on which to check style
#' @param linterList string of linter names (not tags) used to check style
#' @param linterTags single linter tag (not specific linter names) used to check
#' style
#' @description function that uses lintr to check the style format of a single
#' script. The parameters are obtained from instructor by previously using
#' style_input(). This function returns the number of errors in the script for
#' each inputted lintr. These errors will be later used with a weight vector to
#' calculate the final style score.
#' NOTE: Use linterList or linterTags, not both at same time. Consider changing
#' fucntion to only take one parameter for linters, regardless of whether linter
#' names or tags.
#' @return This function returns the number of errors in the script for
#' each inputted lintr. These errors will be later used with a weight vector to
#' calculate the final style score.
#' @example \dontrun {
#' style_check_file(filename = '/submissions/student1.R',
#' linterList = c("assignment_linter", "commas_linter"),
#' linterTags = "best_practices")
#' }
#' @export
style_check_file <- function(filename, linterList = NULL, linterTags = NULL) {

  # Get the list of available linters and tags from lintr
  avail_linters <- available_linters()
  avail_tags <- available_tags()

  # Lints using linter names (not tags)
  if (!is.null(linterList)) {
    # If cannot find a linter name, throw an error.
    if(!all(linterList %in% avail_linters$linter)) {
      stop(paste0(linterList[!(linterList %in% avail_linters$linter)],
                  " not found."))
    }
    # to_check are linters to check
    to_check <- avail_linters[
      which(avail_linters$linter %in% linterList), ]$linter
    linted <- lint(filename)
  }

  # Lints using linter tags (ie. "best_practices" or "common_mistakes", etc.")
  if (!is.null(linterTags)) {
    # If cannot find a linter tag, throw an error.
    if(!all(linterTags %in% avail_tags)) {
      stop(paste0(linterTags[!(linterTags %in% avail_tags)],
                  " not found."))
    }
    # Find linter names that are associated with the inputted linter tag
    # First grab linter tags from avail_linters
    temp <- data.frame(matrix(data = NA, nrow = nrow(avail_linters ), ncol = 1))
    for (i in 1:nrow(temp)) {
      temp[i,1] <- paste(unlist(avail_linters[i, "tags"]), collapse = ", ")
    }
    # to_check are the linter names associated with the linter tags being used
    to_check <- avail_linters[which(grepl(linterTags, temp[,1])), "linter"]
    linted <- lint(filename, linters = (linters_with_tags(tags = linterTags)))
  }

  # Counting instances of errors to be used in score
  errors <- rep(0, length(to_check))
  errors <- matrix(errors, nrow = 1, ncol = length(to_check))
  rownames(errors) <- gsub(".*/", "", filename)
  colnames(errors) <- to_check

  for(m in linted) {
    issue <- colnames(errors)[which(to_check %in% m$linter)]
    errors[, issue] <- errors[, issue] + 1
  }

  # Alternative ideas: consider returning a list with scores and data frame of
  # written feeback. Maybe written feedback should be saved as a csv and then
  # csv can be later used during combine.grading(). If written feedback is
  # incorporated here, then style_feedback() becomes obsolete)

  return(errors)
}

#' @title Function returns written style feedback for a single script
#' @param filename script path used to generate style feedback
#' @param linter string of linter names or tags used to check style. This is
#' provided by style_input(). The default is to use all linters found in
#' available_linters(). NOTE: Consider creating a csv of desired linters as
#' documented in lint(). Current implementation runs lint() using all linters,
#' and we only select the desired ones.
#' style
#' @description function that uses lintr to check the style format of a single
#' script. The parameters are obtained from instructor by previously using
#' style_input(). This function returns a data frame with the first instance of
#' each style error, along with the line number of which it occurred and a
#' message on how to properly style that code.
#' @return This function returns a data frame with the first instance of
#' each style error, along with the line number of which it occurred and a
#' message on how to properly style that code.
#' @example \dontrun {
#' style_check_file(filename = '/submissions/student1.R',
#' linter = c("assignment_linter", "commas_linter")
#' }
#' @export
# Return specific written feedback info for each student (not scores)
style_feedback <- function(filename, linter = available_linters()) {
  # Get the list of available linters and tags from lintr
  avail_linters <- available_linters()
  avail_tags <- available_tags()

  # Lints using linter names (not tags)
  if (names(linter) == "linter_names") {
    # If cannot find a linter name, throw an error.
    if(!all(linter$linter_names %in% avail_linters$linter)) {
      stop(paste0(linter[!(linter$linter_names %in% avail_linters$linter), ],
                  " not found."))
    }
    # to_check are linters to check
    to_check <- avail_linters[
      which(avail_linters$linter %in% linter$linter_names), ]$linter
    linted <- lint(filename)
  }
  # Lints using linter tags (ie. "best_practices" or "common_mistakes", etc.")
  if (names(linter) == "tags") {
    # If cannot find a linter tag, throw an error.
    if(!all(linter$tags %in% avail_tags)) {
      stop(paste0(linter[!(linter$tags %in% avail_tags), ],
                  " not found."))
    }
    # Find linter names that are associated with the inputted linter tag
    # First grab linter tags from avail_linters
    temp <- data.frame(matrix(data = NA, nrow = nrow(avail_linters), ncol = 1))
    for (i in 1:nrow(temp)) {
      temp[i,1] <- paste(unlist(avail_linters[i, "tags"]), collapse = ", ")
    }
    # to_check are the linter names associated with the linter tags being used
    to_check <- avail_linters[which(grepl(linter$tags, temp[,1])), "linter"]
    linted <- lint(filename, linters = (linters_with_tags(tags = linter_names)))
  }

  # Data frame of errors for each linter. Only display first instance of each
  # error. NOTE: Is there a better way to display feedback? Maybe a csv of all
  # linters with their examples/messages, and just pull from csv based on which
  # errors made in the script?
  fb <- as.data.frame(linted)
  fb <- fb[which(fb$linter%in% to_check), ]
  row.names(fb) <- NULL
  # Only keep first unique instance of lintr error detected
  fb <- fb[na.omit(match(to_check, fb$linter)), ]
  fb <- fb[, c("line_number", "message", "linter")]
  fb <- fb[order(fb$linter, decreasing = FALSE) , ]
  row.names(fb) <- NULL

  return(fb)
}

#' @title Function that calculates final style score
#' @param errors Data frame of style errors for a directory of files (not a
#' single file)
#' @description function that uses the data frame of errors that is returned
#' from style_check_file() and then multiplies it by to a weight vector that
#' penalizes based on how common style errors were in a directory of files (ie.
#' more common errors are penalized less harshly, while rare style errors are
#' harshly penalized). NOTE: Consider turning elements of `errors` into a
#' proportion of errors based on number of lines of code in a script. Also,
#' consider incorporating "consistency" of style errors across all questions
#' into final style score.
#' @return This function returns the final style scores for a directory of
#' scripts.
#' @example \dontrun {
#' assign_grade(errors = data.frame(
#' student1: c(assignment_linter = 1, commas_linter = 3),
#' student2: c(assignment_linter = 4, commas_linter = 1))
#' }
#' @export
assign_grade <- function(errors) {
  # Total errors per linter
  temp <- colSums(errors)
  # weightVec depends on distribution of errors across all students
  weightVec <- 1 - (temp / sum(temp))
  for (i in 1:length(weightVec)) {
    if (weightVec[i] == 1) { weightVec[i] <- 0}
  }
  # Score is calculated as a number to subtracted from total allocated points,
  # which currently doesn't exist (instructor would put that in  stylemethod.R)
  score <- (as.matrix(errors) %*% weightVec) * -1

  return(score)
}
