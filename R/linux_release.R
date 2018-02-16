#' Function to get Linux release information. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
linux_release <- function() {
  
  # Get and clean release call
  text <- system("lsb_release -a", intern = TRUE, ignore.stderr = TRUE)
  
  # Filter
  text <- grep("^LSB", text, invert = TRUE, value = TRUE)
  text_split <- stringr::str_split_fixed(text, ":\t", 2)
  
  # Create empty data frame with correct number of variables
  df <- data.frame(matrix(NA, nrow = 1, ncol = length(text_split[, 1])))
  
  # Give names
  names(df) <- threadr::str_to_underscore(text_split[, 1])
  
  # Add values to data frame
  df[1, ] <- text_split[, 2]
  
  return(df)
  
}


#' @export
ubuntu_release <- function() {
  
  .Deprecated(new = "linux_release")
  linux_release()
  
}
  