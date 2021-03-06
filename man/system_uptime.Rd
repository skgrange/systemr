% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system_uptime.R
\name{system_uptime}
\alias{system_uptime}
\title{Function to get system uptime.}
\usage{
system_uptime(round = TRUE, as.vector = FALSE)
}
\arguments{
\item{round}{Should the dates and times be rounded to the nearest second?}

\item{as.vector}{Should the return be a single numeric value indicating 
seconds since boot time? This is the most efficient way to get this value and 
is suitable for multiple/continuous calling in a logging application.}
}
\value{
Tibble or numeric vector.
}
\description{
Function to get system uptime.
}
\author{
Stuart K. Grange
}
