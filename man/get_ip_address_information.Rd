% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ip_address_information.R
\name{get_ip_address_information}
\alias{get_ip_address_information}
\title{Function to get public IP address information from the 
\url{http://ip-api.com} API.}
\usage{
get_ip_address_information(as_list = FALSE)
}
\arguments{
\item{as_list}{Should a list be returned with no manipulation -- the data 
that the API returns.}
}
\value{
Tibble or named list.
}
\description{
Function to get public IP address information from the 
\url{http://ip-api.com} API.
}
\examples{

# Get ip address information
get_ip_address_information()

}
\author{
Stuart K. Grange
}
