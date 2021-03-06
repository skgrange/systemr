% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system_host_reachable.R
\name{system_host_reachable}
\alias{system_host_reachable}
\title{Function to test of a URl/host is reachable.}
\usage{
system_host_reachable(url, timeout = 1)
}
\arguments{
\item{url}{URL of host.}

\item{timeout}{Ping's timeout.}
}
\value{
Logical vector with length of 1.
}
\description{
Function to test of a URl/host is reachable.
}
\examples{

# Ping google
system_host_reachable("google.com")

}
