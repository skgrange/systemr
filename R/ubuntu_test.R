#' Function to test if the current system is an Ubuntu distribution. 
#' 
#' @return Logical vector with length of 1. 
#' 
#' @export
ubuntu_test <- function() {
  
  # Check
  stopifnot(os() == "unix")
  
  # Get release information
  df <- linux_release()
  
  # Test column wise
  test_vector <- purrr::map_lgl(df, ~grepl("ubuntu", ., ignore.case = TRUE))
  
  # Return a logical
  if (any(test_vector)) {
    
    x <- TRUE
    
  } else {
    
    x <- FALSE
    
  }
  
  return(x)
  
}
