#' Function to get the total size of a directory. 
#' 
#' \code{system_directory_size} uses the \code{du} system programme. 
#' 
#' @param directory Vector of directories. 
#' 
#' @param unit Vector of units. Can be \code{"kb"}, \code{"mb"}, or \code{"gb"}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
system_directory_size <- function(directory, unit = "kb") {
  
  # Check
  stopifnot(os() == "unix")
  
  # Parse
  unit <- stringr::str_to_lower(unit)
  directory <- path.expand(directory)
  directory <- shQuote(directory)
  
  # Do
  df <- purrr::map2_dfr(directory, unit, system_directory_size_worker)
  
  return(df)
  
}


system_directory_size_worker <- function(directory, unit) {
  
  # System command work
  command <- stringr::str_c("du -sk ", directory)
  text <- system(command, intern = TRUE)
  x <- stringr::str_split_fixed(text, "\t", 2)[, 1]
  x <- as.numeric(x)
  
  # Convert units, du returns value in kb
  if (unit == "mb") x <- x / 1e+03
  if (unit == "gb") x <- x / 1e+06
  
  # Build data frame to return
  df <- tibble(
    directory = directory, 
    size = x,
    unit = stringr::str_to_upper(unit)
  )
  
  return(df)
  
}
