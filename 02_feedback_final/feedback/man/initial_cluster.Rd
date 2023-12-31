% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Initial_Clustering.R
\name{initial_cluster}
\alias{initial_cluster}
\title{Create clusters based on professor-provided solutions}
\usage{
initial_cluster(
  sol_path = ".",
  student_path = ".",
  func_wrap = FALSE,
  threshold = 0.5
)
}
\arguments{
\item{sol_path}{the path containing all solution library solutions as .R
files}

\item{student_path}{the path containing all student solutions as .R files}

\item{func_wrap}{if \code{TRUE}, will wrap all .R files in both paths in a
dummy function}

\item{threshold}{the default similarity threshold. Must be in (0, 1)}
}
\value{
a list of three objects. Object 1 is a list of character vectors,
with the name of each vector being a professor-provided solution, and vector
elements being the student solutions that clustered with that professor-
provided solution. Object 2 is the current similarity matrix, which will be
used in \code{final_cluster()}. Object 3 is the original similarity matrix
containing pairwise similarity scores of all files, which will be used to
generate the clustering report.
}
\description{
Create clusters based on professor-provided solutions
}
