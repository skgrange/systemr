#' Function to list or count users who have running processes. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character or integer vector. 
#' 
#' @export
system_users_running_processes <- function(count = FALSE) {
  
  if (count) {
    
    # x <- system("who | cut -d \" \" -f 1 | sort -u | wc -l", intern = TRUE)
    x <- system("ps -eaho user | sort -u | wc -l", intern = TRUE)
    x <- as.integer(x)
    
  } else {
    
    # x <- system("who | cut -d \" \" -f 1 | sort -u", intern = TRUE)
    x <- system("ps -eaho user | sort -u", intern = TRUE)
    
  }
  
  return(x)
  
}


#' Function to count number of processes running. 
#' 
#' @return Integer vector with length of 1. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
system_running_processes <- function()
  as.integer(system("ps aux | wc -l", intern = TRUE))
