#' Function to return a system's memory information. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.
#' 
#' @export
system_memory_information <- function() {
  
  # Get text
  text <- readLines("/proc/meminfo")
  
  # Split variables and clean
  matrix_text <- stringr::str_split_fixed(text, ":", 2)
  matrix_text <- apply(matrix_text, 2, stringr::str_trim)
  
  # Special character cleaning
  matrix_text[, 1] <- stringr::str_replace_all(matrix_text[, 1], "\\(|\\)", " ")
  matrix_text[, 1] <- threadr::str_to_underscore(matrix_text[, 1])
  matrix_text[, 1] <- stringr::str_replace(matrix_text[, 1], "_$", "")
  
  # 
  matrix_values <- stringr::str_split_fixed(matrix_text[, 2], " ", 2)
  matrix_values[, 2] <- ifelse(matrix_values[, 2] == "", NA, matrix_values[, 2])
  
  # Build data frame
  df <- data.frame(
    variable = matrix_text[, 1],
    value = as.numeric(matrix_values[, 1]),
    unit = matrix_values[, 2],
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}


#' Function to calculate percentage memory usage. 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Numeric vector with length of one. 
#' 
#' @export
system_memory_usage <- function() {
  
  # Get information
  df <- system_memory_information() %>% 
    dplyr::filter(variable %in% c("mem_total", "mem_available"))
  
  # Calculate percentage used
  x <- df$value[2] / df$value[1]
  x <- (1 - x) * 100
  
  # Reduce percision
  x <- round(x, 1)
  
  return(x)
  
}
