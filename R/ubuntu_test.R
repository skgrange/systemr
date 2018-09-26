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
  test_vector <- purrr::map_lgl(df, ~grepl("ubuntu|mint", ., ignore.case = TRUE))
  
  # Return a logical
  x <- ifelse(any(test_vector), TRUE, FALSE)

  return(x)
  
}
