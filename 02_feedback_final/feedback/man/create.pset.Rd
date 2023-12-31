% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ND_funcs_1202.R
\name{create.pset}
\alias{create.pset}
\title{Merges header.R and pset_body.R to create a problem set as
a pset.R; if any question data/ is non-empty, then create a pset
folder containing pset.R and data/ that could then be zipped and
deployed easily.}
\usage{
create.pset(dir = NA, psetname = "pset.R", psetfolder = "pset")
}
\arguments{
\item{dir}{directory path for the problem set, or use the current directory
if this is missing}

\item{psetname}{psetname}

\item{psetfolder}{psetfolder}
}
\description{
Merges header.R and pset_body.R to create a problem set as
a pset.R; if any question data/ is non-empty, then create a pset
folder containing pset.R and data/ that could then be zipped and
deployed easily.
}
