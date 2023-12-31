% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split.R
\name{split_script}
\alias{split_script}
\title{Splits solution file and all submissions into question level folders}
\usage{
split_script(sol_dir, sub_dir)
}
\arguments{
\item{sol_dir}{directory path containing the mastersolution.R}

\item{sub_dir}{directory path containing all submissions}
}
\description{
function splits solution and submission scripts (using specific
tags within scripts (ie. ### ProblemStartsHere ###, ### ProblemEndsHere ###).
It then creates a directory within the scratch_folder for each question,
(ie. grades-Q1/, sol_dir-Q1/, submission-Q1/).
}
