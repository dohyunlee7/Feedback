#' @title Instructor inputs which linters to later use in style_check_file()
#' @description This function displays the available linters (or linter tags) to
#' the instructor and prompts the instructor to input which linters to use. It
#' returns the inputted linters/tags to then be used in style_check_file().
#' NOTE: Consider whether this function can be completely moved into
#' style_check_file(). Parameters are TBD. Right now, no parameters
#' @return Returns the inputted linters/tags to then be used in style_check_file().
#' @example \dontrun{}
#' @export
style_input <- function() {
  # Prompt instructor whether to use specific linter names, tags, or all
  # available linters
  x <- readline(
    paste0("Which style linters do you want to check?\n",
           "Type 0 to select preset linters based on tags, or\t\n",
           "Type / to use all available linters, or\n",
           "Type # to select individual linters: "))

  while (x != "0" && x != "/" && x != "#") {
    x <- readline(
      paste0("\n\nInput not recognized.\n",
             "Type 0 to select preset linters based on tags, or\t\n",
             "Type / to use all available linters, or\n",
             "Type # to select individual linters: "))
  }

  # For selecting linter tags
  if (x == 0) {
    temp <- as.data.frame(available_tags())
    row.names(temp) <- NULL
    print(temp)
    x <- readline(
      "Input a single row number of the preset linters to use based on the tag names above: ")
    # Need to alter code to allow for more than one tag to be inputted
    linter_names <- temp[x, "available_tags()"]
    # Named as 'tags' as this will be used in style_check_file()
    return(data.frame(tags = linter_names))
  }

  # To use all available linters
  else if (x == "/") {
    temp <- available_linters()
    row.names(temp) <- NULL
    x <- 1:nrow(temp)
    linter_names <- temp[x, "linter"]
    # Maybe do print the next two lines. Is there a better way to display this?
    # print("\nThe following are all the linters that will be used:")
    # print(linter_names)
    # Named as 'linter_names' as this will be used in style_check_file()
    return(data.frame(linter_names = linter_names))
  }

  # For selecting specific linters by name
  else if (x == "#") {
    temp <- available_linters()
    row.names(temp) <- NULL
    # Is there a better way to display this?
    print(temp)
    x <- readline(
      "Based on the linters displayed above, input the row numbers of the linters to used separated by commas: ")
    x <- as.numeric(unlist(strsplit(x, ",")))
    linter_names <- temp[x, "linter"]
    # Named as 'linter_names' as this will be used in style_check_file()
    return(data.frame(linter_names = linter_names))
  }

  # Alternative idea: maybe in script or csv, instructor can select what to
  # check using 1s and 0s.
}
