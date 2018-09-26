#' Function to test of a URl/host is reachable. 
#' 
#' @param url URL of host.
#' 
#' @param timeout Ping's timeout. 
#' 
#' @return Logical vector with length of 1. 
#' 
#' @examples 
#' 
#' # Ping google
#' system_host_reachable("google.com")
#' 
#' @export
system_host_reachable <- function(url, timeout = 1) {
  
  # Build command
  command <- stringr::str_c("ping ", url)
  
  # Ping host
  suppressWarnings(
    text <- system(command, intern = TRUE, timeout = timeout, ignore.stderr = TRUE)
  )
  
  # Test
  x <- ifelse(length(text) == 0, FALSE, TRUE)
  
  return(x)
  
}
