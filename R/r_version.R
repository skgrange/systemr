#' Function to return a formatred \code{R.Version} call. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble with one row. 
#' 
#' @export
r_version <- function() {
  
  # Get version
  list_version <- R.Version()
  
  # Build data frame
  df <- threadr::build_zero_row_tibble(names(list_version))
  df[1, ] <- as.character(list_version)
  
  # Clean names
  names(df) <- stringr::str_replace_all(names(df), "\\.| ", "_")
  
  # Add some formatted strings too
  df <- df %>% 
    mutate(version_string_short = stringr::str_c(language, " ", major, ".", minor),
           version_string_short = stringr::str_c(version_string_short, " ", nickname))
  
  return(df)
  
}
