#' Function to return logged in users. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
system_who <- function() {
  
  # System call
  text <- system("who", intern = TRUE)
  
  # Clean return 
  text <- stringr::str_squish(text)
  list_text <- stringr::str_split(text, " |\\(|\\)")
  list_text <- purrr::map(list_text, ~.x[.x != ""])
  
  # Extract and clean variables
  name <- purrr::map_chr(list_text, 1)
  line <- purrr::map_chr(list_text, 2)
  time <- purrr::map_chr(list_text, ~stringr::str_c(.x[3], " ", .x[4]))
  time <- lubridate::ymd_hm(time, tz = "UTC")
  comment <- purrr::map_chr(list_text, 5)
  
  # Build data frame
  df <- tibble(name, line, time, comment)
  
  return(df)
  
}
