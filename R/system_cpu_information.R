#' Function to return CPU information. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
system_cpu_information <- function() {
  
  # Read file
  text <- readLines("/proc/cpuinfo", warn = FALSE)
  text <- unique(text)
  
  # Split into variables and values
  matrix_split <- stringr::str_split_fixed(text, "\t:", 2)

  # Clean names
  names <- matrix_split[, 1]
  names <- stringr::str_replace_all(names, "\t", "")
  names <- stringr::str_to_lower(names)
  names <- make.unique(names, sep = "_")
  names <- stringr::str_replace_all(names, " ", "_")
  names <- ifelse(names == "", "to_drop", names)
  
  # Get values
  values <- matrix_split[, 2]
  values <- stringr::str_trim(values)
  
  # Create empty data frame with correct number of variables
  df <- data.frame(
    matrix(0, ncol = length(names), nrow = 1)
  )
  
  # Give names
  names(df) <- names
  
  # Give values
  df[1, ] <- values
  
  # Drop empty variables
  df[, "to_drop"] <- NULL
  
  # Type convert
  df[] <- lapply(df, function(x) type.convert(x, as.is = TRUE))
  
  # To tibble
  df <- as_tibble(df)
  
  return(df)
  
}
