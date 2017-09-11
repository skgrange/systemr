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
#' @return Data frame. 
#' 
#' @export
system_directory_size <- function(directory, unit = "kb") {
  
  # Check
  stopifnot(os() == "unix")
  
  # Parse
  unit <- stringr::str_to_lower(unit)
  directory <- shQuote(directory)
  
  df_map <- data.frame(
    directory, 
    unit, 
    stringsAsFactors = FALSE
  )
  
  # Do, rowwise
  df <- plyr::adply(
    df_map, 
    .margins = 1,
    system_directory_size_worker,
    .progress = "none"
  )
  
  return(df)
  
}


system_directory_size_worker <- function(df_map, unit = "kb") {
  
  # System command work
  command <- stringr::str_c("du -sk ", df_map$directory)
  text <- system(command, intern = TRUE)
  x <- stringr::str_split_fixed(text, "\t", 2)[, 1]
  x <- as.numeric(x)
  
  # Convert units, du returns value in kb
  if (df_map$unit == "mb") x <- x / 1e+03
  if (df_map$unit == "gb") x <- x / 1e+06
  
  # Build data frame to return
  df <- data.frame(
    directory = df_map$directory, 
    size = x,
    unit = stringr::str_to_upper(df_map$unit),
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}
