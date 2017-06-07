#' Function to get R's version information. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
r_version <- function() {
  
  # Get information
  list_version <- R.version
  
  # Get pieces of simple.list
  variable <- names(list_version)
  variable <- stringr::str_replace_all(variable, " |\\.", "_")
  values <- as.character(list_version)
  
  # Create empty data frame with correct number of variables
  df <- data.frame(matrix(NA, nrow = 1, ncol = length(variable)))
  
  # Give names
  names(df) <- variable
  
  # Add values to data frame
  df[1, ] <- values
  
  # Add another variable
  df[, "version_full"] <- stringr::str_c(df[, "major"], df[, "minor"], sep = ".")
  
  return(df)
  
}
