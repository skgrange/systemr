#' Function to get Ubuntu release information. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
ubuntu_release <- function() {
  
  # Get and clean release call
  text <- system("lsb_release -a", intern = TRUE)
  text <- text[3:5]
  text_split <- stringr::str_split_fixed(text, ":\t", 2)
  
  # Create empty data frame with correct number of variables
  df <- data.frame(matrix(NA, nrow = 1, ncol = length(text_split[, 1])))
  
  # Give names
  names(df) <- stringr::str_to_lower(text_split[, 1])
  
  # Add values to data frame
  df[1, ] <- text_split[, 2]
  
  return(df)
  
}
