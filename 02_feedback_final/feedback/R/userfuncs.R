#' Feedback and grading system for R coding
#' @param student_dir Path to directory of student submitted solutions
#' @param solution_dir Path to directory of a example solution made by instructor
#' @param timeout timeout threshold.
#' @param ... Option for functions
#' @return Recommended solutions library based on algorithmic, objective, style
#' grade as well as a database of feedback and grades for submitted solutions
#' @examples
#' \dontrun{
#' print("hello")
#' }
#' @export
#'
libraryCheck <- function(student_dir, solution_dir, timeout = 10, ...) {
  # Student Solutions
  xs <- dir(student_dir, full.names = TRUE)
  # Library Solutions
  ss <- dir(solution_dir, full.names = TRUE)
  # Student and Library Solutions
  all <- c(xs, ss)

  # Algorithmic Check // Similarity Check
  algo_score <- algoCheck_all(student_dir, solution_dir)
  # Unique cluster algorithms
  clusters <- unique(apply(algo_score, 1, function(x) which(x == 1)))
  # Sample solution: last indicator in each cluster, do this to minimize runtime
  # If there is no library solution in the cluster, we are currently choosing
  # one arbitrarly; think about it later
  cluster_sample_sol <- unlist(lapply(clusters, max))

  # timeout <- 10
  # Objective Check for each cluster, using sample solution
  obj_checker <- correctCheck_filesdir(all[cluster_sample_sol],
                                       ss[1],   # -------------------------------- Redesign
                                       timeout = timeout)# , ...)
  # Objective Grade for each cluster
  obj_grade <- unname(sapply(lapply(obj_checker, '[[', 'correctCheck'), utils::tail, 1))
  # isTimeout <- sapply(obj_checker, '[[', 'timeout')
  # isCodefailure <- sapply(obj_checker, '[[', 'codefailure')
  # isMessage <- sapply(obj_checker, '[[', 'message')

  #
  # This does only allow for solutions that pass the timeout constraint!
  #

  # Database Creation:
  # FileID in clusterorder
  fileID <- unlist(clusters)
  # In which cluster?
  which_cluster <- rep(1:length(obj_grade), times = lengths(clusters))
  # Grade for cluster
  objective_grade <- rep(obj_grade, lengths(clusters))
  # timeouts <- rep(isTimeout, lengths(clusters))
  # codefailure <- rep(isCodefailure, lengths(clusters))
  # message <- rep(isMessage, lengths(clusters))
  # Bind together
  db <- cbind(fileID, which_cluster, objective_grade) #, timeouts, codefailure)
  # Order by FileID
  db <- db[order(db[, 1]),]

  # Style Grade:
  style_grade <- sample(0:10, length(all), replace = TRUE)

  # Add style to DB:
  db <- cbind(db, style_grade)

  # Find Suggested Library:
  # Initilize vectors
  cand <- numeric(sum(obj_grade == 100))
  match <- numeric(sum(obj_grade == 100))
  for (i in 1:length(cand)) {
    # Solutions in cluster
    sols <- clusters[[which(obj_grade == 100)[i]]]
    # Find if it matches with any of the library solutions
    match_check <- intersect(sols, utils::tail(1:length(all), length(ss)))
    if (length(match_check) == 0) {
      match[i] <- 'New Solution'
    } else {
      match[i] <- all[match_check]
    }
    # Pick the highest style grade as candidate solution
    cand[i] <- sols[which.max(db[sols, 4])]
  }

  # For new library find best mach and the score
  new_library_match <- apply(algo_score[, cand], 1, function(x) which.max(x))
  new_library_match_score <- apply(algo_score[, cand], 1, function(x) max(x))
  # Add best match in new library and score to db
  db <- cbind(db, new_library_match, new_library_match_score)

  # New Library and the match
  new_library <- data.frame('Suggested_Library' = all[cand],
                            'Match' = match)
  return(list(Database = db, Library = new_library))
}
