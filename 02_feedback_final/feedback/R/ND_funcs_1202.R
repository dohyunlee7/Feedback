#' @title Create a skeleton for a new question in the QLibrary folder
#' @param dir path to a directory where skeleton for a new question will be
#' added to the question library (QLibrary)
#' @param qfile path to a script that follows the "rules" and is used for
#' the statement.R file.  This is optional.
#' @description function creates these blank files and folders:
#' statement.R - the question which a student shall solve
#' solutions/ - folder to store the solution library
#' solutions/mastersolution.R - instructor solution
#' submission_archive/ - folder to store all submitted student scripts
#' data/ - if the question involves a data set it shall be stored here
#' correctmethod.R (optional, TBD for now)
#' algomethod.R (optional, TBD for now)
#' stylemethod.R (for desired style formatting)
#' @export
question.skeleton <- function(dir = NA, qfile = NA){

  # Check if dir exists
  if(!dir.exists(dir)) {
    prompt <- askYesNo(paste('Directory not found, do you want to create',
                             'the new question in your current directory?',
                             sep = ' '))
    if (!prompt) {
      stop('Pick a new path')
    }
  }

  # Set dir as current directory
  if (is.na(dir)) {
    dir <- getwd()
  }

  # Set new name for as
  ## does this work for all machines? Windows? Ask Jay.
  created.dir <- paste0(dir, '/NewQuestion')

  # Check if dir exists
  # ------ Note: Breaks if it exists, do we want to force instructor to change
  # ------ the names of the directory here if NewQuestion/ exists?
  # ------ We could also assign a unique name? but maybe the other suggestion
  # ------ makes more sense? Ask Jay
  if(dir.exists(created.dir)) {
    stop(paste('Directory "NewQuestion" already exists,',
               'you should change the name of the folder',
               sep = ' '))
  }

  dir.create(created.dir)

  # Check if instructor wants to use an old file for statement.R
  if (is.na(qfile)) {
    statement <- paste0(created.dir, '/statement.R')
    file.create(statement)
  } else if (file.exists(qfile)) {
    file.copy(from = qfile, to = created.dir)
    file_from <- paste0(created.dir, '/', dir(created.dir))
    file_to <- paste0(created.dir, '/', 'statement.R')
    file.rename(file_from, file_to)
  } else {
    message('No, qfile found at path given. blank statement.R created instead')
  }

  # Set up creation of files in NewQuestion
  algomethod <- paste0(created.dir, '/algomethod.R')
  correctmethod <- paste0(created.dir, '/correctmethod.R')
  stylemethod <- paste0(created.dir, '/stylemethod.R')
  # Set up creation of folders in NewQuestion
  data.dir <- paste0(created.dir, '/data')
  solutions.dir <- paste0(created.dir, '/solutions')
  submissions.dir <- paste0(created.dir, '/submission_archive')
  # Set up creation of file in solutions.dir
  mastersolution <- paste0(solutions.dir, '/mastersolution')

  # Create files in NewQuestion
  file.create(algomethod)
  file.create(correctmethod)
  file.create(stylemethod)
  # Create folders in NewQuestion
  dir.create(data.dir)
  dir.create(solutions.dir)
  dir.create(submissions.dir)
  # Create file in solutions/
  file.create(mastersolution)
  message('New question created in question library')
  message('Remember to provide statement, data and potentially something else')
}

#! Instructor has to write the question such that the skeleton becomes a "body".
#! When prompted, a student should be able to look at statement.R and know what
#! needs to be done.  The instructor also needs at least one solution,
#! mastersolution.R, and potentially more if available/desired.  Maybe the
#! instructor provides a needed data set or sets.  The instructor might also
#! specify some options for correcting or algo work TBD.

#? We need a system (set of rules) for including scores and feedback in
#? files of solutions/.

#! To create a problem set, the instructor must specify which questions
#! are to be used.  So that's next...

#' @title Create a skeleton for a new problem set in a folder that does not live
#' in the QLibrary folder
#' @param dir directory path for a new problem set (scratch folder)
#' @param qdirs vector of directory paths to each question that the instructor
#' wants to use for this problem set, ordered appropriately
#' @description function creates these blank files and folders:
#' - header.R - a header and an introduction to the problem set
#' - stylemethod.R - instructions to feedback on how to evaluate style
#' - other.R (TBD)
#' - pset_body.R - the assembled questions in blocks
#' - q_names.csv - a csv file that lines up with pset_body.R, giving the
#' proper order, question names (unique at the level of this problem set),
#' and paths to the question folders in QLibrary.
#' - Make copies of the QLibrary question folders in this specified
#' scratch folder, dir, and only at the very very end, and very carefully,
#' would we "push back" to the QLibrary folders.
#' - ?SKIP? Make a pset-level data/ folder, with all data from the
#' question folders
#' @export
pset.skeleton <- function(dir, qdirs) {

  # Check if dir exists
  if(!dir.exists(dir)) {
    prompt <- askYesNo(paste('Directory not found, do you want to create',
                             'the new pset in your current directory?',
                             sep = ' '))
    if (!prompt) {
      stop('Pick a new path')
    }
  }

  # Set dir as current directory
  if (is.na(dir)) {
    dir <- getwd()
  }

  # Set new name for as
  ## does this work for all machines? Windows? Ask Jay.
  created.dir <- paste0(dir, '/scratch_pset')

  # Check if dir exists
  # ------ Note: Breaks if it exists, do we want to force instructor to change
  # ------ the names of the directory here if NewQuestion/ exists?
  # ------ We could also assign a unique name? but maybe the other suggestion
  # ------ makes more sense? Ask Jay
  if(dir.exists(created.dir)) {
    stop(paste('Directory "scratch_pset" already exists, you should',
               'change the name of the folder', sep = ' '))
  }

  # Create directory for pset_skeleton called scratch_pset ---- for now
  dir.create(created.dir)
  # Create new directory in scratch_pset for individual questions
  new_q_dir <- paste0(created.dir, '/questions')
  dir.create(new_q_dir)

  # How many questions are there in the new pset?
  n <- length(qdirs)
  # Initilize names for each question
  q_names <- matrix(NA, nrow = n, ncol = 2)
  q_statements <- vector('list', n)

  for (i in 1:n) {
    # One question at a time
    tmp_q <- qdirs[i]

    # Check if any statement.R
    check <- dir(tmp_q) == 'statement.R'

    # Check if there is a statement.R in the folder, required to build pset
    if(length(check) == 0 | (!any(check))) {
      stop(paste(tmp_q,
                 'is invalid, try another path for that question.',
                 sep =' '))
    }

    # Store the statements from each of the questions
    q_statements[[i]] <- scan(paste0(tmp_q, '/', dir(tmp_q)[check]),
                              what = "", sep = "\n", quiet = TRUE)

    # Copy the data from question directory to pset data directory but only
    # if there is any data to be found
    if(any(dir(tmp_q) == 'data') & (length(dir(paste0(tmp_q, '/data'))) > 0)) {

      # Create data folder if it hasn't already been created
      if (!any(dir(created.dir) == 'data')) {
        dir.create(paste0(created.dir, '/data'))
      }

      # Copy question data folder to pset data folder
      file.copy(paste0(tmp_q, '/data'),
                paste0(created.dir, '/data'),
                recursive = TRUE)

      # Rename within data folder to match questions
      file.rename(paste0(created.dir, '/data/data'),
                  paste0(created.dir, '/data', '/q_', i))
    }

    # Copy entire folder to our new "questions/"
    file.copy(tmp_q, new_q_dir, recursive = TRUE)

    # Rename within questions
    name <- sub(".*/", "", tmp_q)
    file.rename(paste0(new_q_dir, '/', name),
                paste0(new_q_dir, '/q_', i))

    # Add old names and new names to q_names
    q_names[i, ] <- c(tmp_q, paste0(new_q_dir, '/q_', i))
  }

  # If all paths had a file with statement.R then we create the following
  file.create(paste0(created.dir, '/header.R'))
  file.create(paste0(created.dir, '/stylemethod.R'))
  write.csv(q_names, paste0(created.dir, '/q_names.csv'))
  file.create(paste0(created.dir, '/pset_body.R'))

  # Write the statements in the pset_body
  writeLines(text = unlist(q_statements),
             con = paste0(created.dir, '/pset_body.R'),
             sep = '\n\n')

  message('New problem set created')
  message('Remember to write the header and potentially something else')
}

#! Next, the instructor has to write the header and possible set options.
#! Then:

#' @title Merges header.R and pset_body.R to create a problem set as
#' a pset.R; if any question data/ is non-empty, then create a pset
#' folder containing pset.R and data/ that could then be zipped and
#' deployed easily.
#' @param dir directory path for the problem set, or use the current directory
#' if this is missing
#' @param psetname psetname
#' @param psetfolder psetfolder
#' @export
create.pset <- function(dir = NA, psetname = "pset.R", psetfolder = "pset") {

  # Check if directory was provided
  if(is.na(dir)) {
    stop('Directory not provided')
  }

  # Check if pset_body.R is in provided directory
  if(!any(dir(dir) == 'pset_body.R')){
    stop('No pset_body.R found')
  }

  # Check if header.R is in provided directory
  if(!any(dir(dir) == 'header.R')){
    stop('No header.R found')
  }

  # Scan pset_body.R
  pset_body <- scan(paste0(dir, '/', 'pset_body.R'),
                    what = "", sep = "\n", quiet = TRUE)

  # Scan header.R
  header <- scan(paste0(dir, '/', 'header.R'),
                 what = "", sep = "\n", quiet = TRUE)

  # Check if the header is empty
  if(identical(header, character(0))) {
    prompt <- askYesNo(paste('The header.R file is empty',
                             'Do you want to write a header for your problem',
                             'set or do you want to keep on going',
                             sep = ' '))
    if (!prompt) {
      stop('Go write a header and then come back and run this function')
    }
  }

  if(any(dir(dir) == 'data') & (length(dir(paste0(dir, '/data'))) > 0)) {
    # Create problem set folder
    dir.create(paste0(dir, '/pset'))
    dir.create(paste0(dir, '/pset/data'))

    # Copy data folder to our new pset folder
    file.copy(paste0(dir, '/data'),
              paste0(dir, '/pset'),
              recursive = TRUE)

    # Set pset_path as our newly created folder
    pset_path <- paste0(dir, '/pset')

  } else {

    # Set pset_path as directory
    pset_path <- dir
  }

  # Create pset.R in pset_path
  file.create(paste0(pset_path, '/pset.R'))

  # Write the header and pset_body in pset.R
  writeLines(text = unlist(list(header, pset_body)),
             con = paste0(pset_path, '/pset.R'),
             sep = '\n\n')

}

