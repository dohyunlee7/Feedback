#' Create clusters based on professor-provided solutions
#' 
#' @param sol_path the path containing all solution library solutions as .R 
#' files
#'
#' @param student_path the path containing all student solutions as .R files
#'
#' @param func_wrap if \code{TRUE}, will wrap all .R files in both paths in a 
#' dummy function
#' 
#' @param threshold the default similarity threshold. Must be in (0, 1)
#'
#' @return a list of three objects. Object 1 is a list of character vectors, 
#' with the name of each vector being a professor-provided solution, and vector
#' elements being the student solutions that clustered with that professor-
#' provided solution. Object 2 is the current similarity matrix, which will be 
#' used in \code{final_cluster()}. Object 3 is the original similarity matrix 
#' containing pairwise similarity scores of all files, which will be used to 
#' generate the clustering report.
#' 
#' @export


initial_cluster <- function(sol_path = ".", 
                            student_path = ".", 
                            func_wrap = FALSE,
                            threshold = 0.5) {
  
  # To retrieve the function parse_sort()
  source("Cleaning.R")
  library(SimilaR)
  
  # Clean the student solution files using the parse_sort() function
  test <- parse_sort(sol_path = sol_path,
                     student_path = student_path,
                     func_wrap = func_wrap)
  
  # Move files that do not parse to a new directory since they will break
  # SimilaR functions
  if(length(unlist(test))) {
    dir.create("Not_Running_Files")
    sapply(unlist(test), 
           function(x) file.copy(from = x, to = "./Not_Running_Files/"))
    file.remove(unlist(test))
  }
  
  
  # Temporarily move the files from solutions to submissions folder to create
  # the similarity matrix
  sol_files <- list.files(sol_path, full.names = TRUE)
  sol_files_names <- list.files(sol_path)
  sapply(sol_files, function(x) file.copy(from = x, to = student_path))
  
  # Now that the files have been cleaned, run and create our similarity matrix. 
  sim_mat <- SimilaR_fromDirectory(dirname = student_path,
                                   returnType = "data.frame",
                                   fileTypes = "file",
                                   aggregation = "tnorm")
  sim_mat$SimilaR <- as.numeric(sim_mat$SimilaR)
  
  # Delete the built-in similarity classifier scores, since we won't use them
  sim_mat <- sim_mat[,-c(4)]
  
  # Remove the solution files from the submissions folder 
  file.remove(paste(student_path, "/", sol_files_names, sep = ""))
  
  # Clean the function names to make them easier to work with
  sim_mat$name1 <- trimws(gsub("^(.*) main$", "\\1", sim_mat$name1))
  sim_mat$name2 <- trimws(gsub("^(.*) main$", "\\1", sim_mat$name2))
  
  # Save a copy of the similarity matrix, which will be used to generate the 
  # cluster report
  orig_sim_mat <- sim_mat

  
  # The indices of the similarity matrix where all professor solutions are being
  # compared to each other
  prof_inds <- which(sim_mat$name1 %in% sol_files_names 
                     & sim_mat$name2 %in% sol_files_names)  
  
  # The similarity matrix just containing professor solutions
  prof_mat <- sim_mat[prof_inds,]
  
  # A list of all clusters
  clusters = list()
  
  i <- 1
  while(nrow(prof_mat) != 0) {
    
    # The index of the prof solution matrix with the highest similarity score
    max_prof_ind <- which.max(prof_mat$SimilaR)
    
    # The file names of these prof solutions
    curr_prof_sols <- c(prof_mat[max_prof_ind,]$name1, 
                        prof_mat[max_prof_ind,]$name2)
    
    # So the maximum similarity score between prof solutions is the threshold,
    # or if they are very different, just the default threshold
    curr_threshold <- max(prof_mat[max_prof_ind,]$SimilaR, threshold)
    
    # The indices of the similarity matrix containing the current prof solutions
    inds <- which(sim_mat$name1 %in% curr_prof_sols 
                  | sim_mat$name2 %in% curr_prof_sols)
    
    curr_sim_mat <- sim_mat[inds,]
    curr_sim_mat <- curr_sim_mat[which(curr_sim_mat$SimilaR > curr_threshold),]
    
    # The names of each cluster will be each prof solution
    cluster_name_1 <- curr_prof_sols[1]
    cluster_name_2 <- curr_prof_sols[2]
    
    
    # A list of all current student solutions
    student_sols <- unique(c(curr_sim_mat$name1, curr_sim_mat$name2))
    
    # Remove professor solutions
    student_sols <- student_sols[student_sols != curr_prof_sols[1] 
                                 & student_sols != curr_prof_sols[2]]
    
    # This is the length of a cluster if all student solutions cluster with a 
    # single professor solution
    max_length <- length(student_sols)
    
    # Then make two clusters (vectors) with this length
    cl1 <- rep("", max_length)
    cl2 <- rep("", max_length)
    
    
    
    # Now actually assign each student solution to a cluster 
    j <- 2
    for(student_sol in student_sols) {
      
      # Get indices of curr_sim_mat that contain the student_sol as a name
      student_sol_inds <- which(curr_sim_mat$name1 %in% student_sol 
                                | curr_sim_mat$name2 %in% student_sol)
      
      # Matrix just containing the current student_sol and professor solutions
      student_sol_mat <- curr_sim_mat[student_sol_inds,]
       
      # Index with the higher similarity score
      max_sim_ind <- which.max(student_sol_mat$SimilaR)
      
      # The matrix containing only the current student_sol and professor 
      # solution that it clusters with
      final_mat <- student_sol_mat[max_sim_ind, 1:2]
      
      # Extract the professor solution from the final_mat
      if(final_mat[1,]$name1 == curr_prof_sols[1] 
         | final_mat[1,]$name2 == curr_prof_sols[1]) {
        cl1[j] <- student_sol
      } else {
        cl2[j] <- student_sol
      }
      
      
      j <- j + 1
      
    } # End for
    
    
    # Take out empty elements from clusters 1 and 2
    cl1 <- cl1[cl1 != ""]
    cl2 <- cl2[cl2 != ""]
    
    
    # Actually add the cluster vectors to the cluster list
    clusters[[cluster_name_1]] <- cl1
    clusters[[cluster_name_2]] <- cl2
    
    
    # Remove used student_sols from sim_mat
    rem_inds <- which(!sim_mat$name1 %in% student_sols 
                      & !sim_mat$name2 %in% student_sols)
    sim_mat <- sim_mat[rem_inds,]
    
    
    # Remove used prof sols from prof_mat and sim_mat
    rem_inds <- which(!sim_mat$name1 %in% curr_prof_sols 
                      & !sim_mat$name2 %in% curr_prof_sols)
    sim_mat <- sim_mat[rem_inds,]
    
    rem_inds <- which(!prof_mat$name1 %in% curr_prof_sols 
                      & !prof_mat$name2 %in% curr_prof_sols)
    prof_mat <- prof_mat[rem_inds,]
    
    
    # Remove used professor solutions from sol_files
    sol_files_names <- sol_files_names[sol_files_names != curr_prof_sols[1] 
                                       & sol_files_names != curr_prof_sols[2]]
    
    
    
    i <- i + 2
  } # End while
  
  # Then if there is another professor solution, (i.e., an odd number of prof
  # solutions), cluster using the default threshold
  if(length(sol_files_names) == 1) {
    
    # Indices where sim_mat still has the remaining professor solution
    inds <- which(sim_mat$name1 %in% sol_files_names 
                  | sim_mat$name2 %in% sol_files_names)
    
    # The similarity matrix containing student solutions that cluster with the 
    # original professor solution
    curr_sim_mat <- sim_mat[inds,]
    curr_sim_mat <- sim_mat[which(curr_sim_mat$SimilaR > threshold),]
  
    # Set the cluster name to the prof solution
    cluster_name <- sol_files_names[1]

    # A list of all current student solutions
    student_sols <- unique(c(curr_sim_mat$name1, curr_sim_mat$name2))
    
    # Remove professor solution
    student_sols <- student_sols[student_sols != sol_files_names[1] 
                                 & student_sols !=  sol_files_names[1]]
    
    # Append this cluster to the clusters list
    clusters[[cluster_name]] <- student_sols
    
    

    # Remove last prof solution and matching student solutions from sim_mat
    rem_inds <- which(!sim_mat$name1 %in% sol_files_names[1] 
                      & !sim_mat$name2 %in% sol_files_names[1])
    sim_mat <- sim_mat[rem_inds,]
    
    rem_inds <- which(!sim_mat$name1 %in% student_sols 
                      & !sim_mat$name2 %in% student_sols)
    sim_mat <- sim_mat[rem_inds,]
  }
  
  
  
  return(list(clusters, sim_mat, orig_sim_mat))
  
}



RUN <- TRUE
if(RUN) {
  clusters <- initial_cluster(sol_path = "./Solutions",
                              student_path = "./Submissions",
                              func_wrap = FALSE,
                              threshold = 0.5)
  # The initial clusters
  clusters[[1]]
}


