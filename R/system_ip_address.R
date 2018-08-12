#' Function to return system's IP address. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
system_ip_address <- function() {
  
  text <- system("hostname -I", intern = TRUE)
  x <- stringr::str_split_fixed(text, " ", 2)[1]
  return(x)
  
}
