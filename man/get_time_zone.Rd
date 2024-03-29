% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_time_zone.R
\name{get_time_zone}
\alias{get_time_zone}
\alias{clean_time_zone_table}
\title{Function to get time zone information from a date, latitude, and longitude 
triad from \url{https://timezonedb.com/api}.}
\usage{
get_time_zone(list, key, sleep = 1, verbose = FALSE, progress = FALSE)

clean_time_zone_table(df)
}
\arguments{
\item{list}{List or data frame containing at least \code{date}, 
\code{latitude}, and \code{longitude} elements or variables.}

\item{key}{\code{timezonedb.com} API key.}

\item{sleep}{Number of seconds to sleep between API queries. One second 
between queries is needed when a non-paid version of the API is being used.}

\item{verbose}{Should the function give messages?}

\item{progress}{Should a progress bar be displayed?}

\item{df}{Data frame/tibble return from \code{get_time_zone}.}
}
\value{
Tibble.
}
\description{
To use this function an API key is needed from \url{https://timezonedb.com}.
}
\author{
Stuart K. Grange
}
