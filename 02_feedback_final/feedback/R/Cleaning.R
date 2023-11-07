#' Check if student solutions can be parsed
#' 
#' @param sol_path the path containing all solution library solutions as .R 
#' files
#'
#' @param student_path the path containing all student solutions as .R files
#'
#' @param func_wrap if TRUE, will wrap all .R files in both paths in a dummy 
#' function
#'
#' @return a list of the .R files in the student path that do not parse
#' 
#' @export

parse_sort <- function(sol_path = ".", student_path = ".", func_wrap = FALSE) {
  
  # Start by processing solution library files
  if(func_wrap) {
    add_front <- "main <- function() {\nset.seed(1)\n\n"
    add_end <- "\n}"
    
    # Read in only .R files from a path as a list of file names
    sol_files <- as.list(dir(sol_path, full.names = TRUE, pattern = "\\.[rR]$"))
    
    for(i in 1:length(sol_files)) {
      file_name <- sol_files[[i]]
      
      # Extract the text from the file
      file_text <- readChar(file_name, file.info(file_name)$size)
      
      # Combine the front text, code, and end text
      file_text <- paste(add_front, file_text, add_end)
      
      # Write the modified text back to the file
      writeLines(file_text, file_name)
    }
  }
  
  
  
  # Read in only .R files from a path as a list of file names 
  student_files <- as.list(dir(student_path, 
                               full.names = TRUE, 
                               pattern = "\\.[rR]$"))
  
  # Will (later) be TRUE if a file parses and FALSE if it doesn't 
  does_parse <- rep(TRUE, length(student_files))
  
  
  for(i in 1:length(student_files)) {
    file_name <- student_files[[i]]
    
    if(func_wrap) {
      
      # Extract the text from the file
      file_text <- readChar(file_name, file.info(file_name)$size)
      
      # Combine the front text, file_text, and end text
      file_text <- paste(add_front, file_text, add_end)
      
      # Write the modified text back to the file
      writeLines(file_text, file_name)
    }
    
    try_to_parse <- try(parse(file = file_name))
    
    # If a file does not parse, change its index in 'does_parse' to FALSE
    if (class(try_to_parse) == "try-error") {
      does_parse[i] <- FALSE
    }
    
  } 

  return(list(student_files[!does_parse]))
  
}


RUN <- FALSE
if(RUN) {
   test <- parse_sort(sol_path = "./Solutions",
                      student_path = "./Submissions",
                      func_wrap = FALSE)
   
   test
}

