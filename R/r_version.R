#' Function to return a formatted \code{R.Version} call. 
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
  df <- threadr::build_zero_row_tibble(names(list_version), base = TRUE)
  df[1, ] <- as.character(list_version)
  
  # Clean names and add some formatted strings
  df <- df %>% 
    as_tibble() %>% 
    dplyr::rename_with(~stringr::str_replace_all(., "\\.| ", "_")) %>% 
    mutate(version_major_minor = stringr::str_c(major, ".", minor),
           version_string_short = stringr::str_c(language, " ", version_major_minor),
           version_string_short = stringr::str_c(version_string_short, " ", nickname))
  
  return(df)
  
}
