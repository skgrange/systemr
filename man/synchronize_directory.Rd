% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synchronize_directory.R
\name{synchronize_directory}
\alias{synchronize_directory}
\title{Function to synchronize a local and remote directory with \code{rsync}.}
\usage{
synchronize_directory(
  directory_local,
  directory_remote,
  calculate_size = TRUE,
  dry_run = TRUE,
  delete = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{directory_local}{Local directory to synchronize to remote directory.}

\item{directory_remote}{Remote directory to synchronize from local directory.}

\item{calculate_size}{Should the function calculate and include directory 
size in the return?}

\item{dry_run}{Should \code{rsync} be run in dry run mode to simulate, but 
not conduct the file synchronization. Good for testing.}

\item{delete}{Should files be deleted? \code{synchronize_directory} will 
delete files in the remote directory if not present in the local directory by
default.}

\item{verbose}{Should the function give messages?}
}
\value{
Tibble with programme timing information.
}
\description{
\strong{Please note}, \code{synchronize_directory} will delete files in the 
remote directory if not present in the local directory by default.
}
\author{
Stuart K. Grange
}
