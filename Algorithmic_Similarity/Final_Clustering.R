#' Create clusters from student solutions that remain unclustered after initial
#' clustering takes place
#' 
#' @param sol_path the path containing all professor-provided solutions as .R 
#' files
#'
#' @param student_path the path containing all student solutions as .R files
#'
#' @param func_wrap if \code{TRUE}, will wrap all .R files in both paths in a 
#' dummy function
#' 
#' @param threshold the default similarity threshold. Must be in (0, 1)
#'
#' @return a list of 2 objects. The first object is a list of character vectors, 
#' each containing the clustered student solution file names as elements. Each 
#' vector is given the name of the professor-provided solution or a generic 
#' name, depending if each student solution clustered with the professor-
#' provided solutions or not
#' 
#' @export

final_cluster <- function(sol_path = ".",
                          student_path = ".", 
                          func_wrap = FALSE,
                          threshold = 0.5) {
  
  # To import the initial_cluster() function
  source("Initial_Clustering.R")
  clusters_sim_mat <- initial_cluster(sol_path = sol_path, 
                                     student_path = student_path, 
                                     func_wrap = func_wrap,
                                     threshold = threshold)
  
  # Unpack the output of initial_cluster()
  clusters <- clusters_sim_mat[[1]]
  sim_mat <- clusters_sim_mat[[2]]
  orig_sim_mat <- clusters_sim_mat[[3]]
  
  # A list of all file names in the similarity matrix, for later
  all_names <- unique(c(unique(sim_mat$name1), unique(sim_mat$name2)))
  
  flag <- 1
  i <- 0
  
  while(flag == 1) {
    
    # Check if the largest similarity score is less than the threshold, or if
    # sim_mat is empty, meaning there's no more clustering to be done
    if((as.numeric(sim_mat[1,]$SimilaR) < threshold) | dim(sim_mat)[1] == 0) {
      flag = 0
    } else {
      names <- c(sim_mat[1,]$name1, sim_mat[1,]$name2)
      sim_mat <- sim_mat[-1,]
      
      # Indices corresponding to (pairwise) sufficiently similar files
      temp <- sim_mat[which((sim_mat$name1 %in% names |
                               sim_mat$name2 %in% names) &
                              sim_mat$SimilaR >= threshold),]
      temp <- unique(c(unique(temp$name1), unique(temp$name2)))
      
      # Create the new clusters
      if(length(temp) > 2) {
        i <- i + 1
        clname <- paste("cl", i, sep = "")
        clusters[[clname]] <- temp
        
        # Remove the used student solutions from sim_mat
        sim_mat <- sim_mat[which(!sim_mat$name1 %in% temp &
                                   !sim_mat$name2 %in% temp),]
        sim_mat <- sim_mat[order(sim_mat$SimilaR, decreasing = TRUE),]
      }
      rownames(sim_mat) <- NULL
    }
  }
  
  # Now appending all the remaining students to singular clusters
  
  try <- unname(unlist(clusters))
  all_names <- all_names[which(!all_names %in% names(clusters))]
  
  left_out_names <- all_names[which(!all_names %in% try)]
  
  if (length(left_out_names)) {
    for(j in 1:length(left_out_names)){
      i <- i + 1
      clname <- paste("cl", i, sep = "")
      clusters[[clname]] <- left_out_names[j]
    }
  }
  
  
  return(list(clusters, orig_sim_mat))
}

RUN <- TRUE
if(RUN) {
  clusters <- final_cluster(sol_path = "./Solutions", 
                            student_path = "./Submissions", 
                            func_wrap = FALSE,
                            threshold = 0.5)
  
  # All clusters
  clusters[[1]] 
}






