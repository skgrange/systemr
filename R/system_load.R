#' Function to get system loads.
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
system_load <- function() {
  
  # Get and clean load averages
  text <- system("cat /proc/loadavg", intern = TRUE)
  text_split <- stringr::str_split(text, " ")[[1]][1:3]
  text_split <- as.numeric(text_split)
  
  # Get cores
  cores <- parallel::detectCores()
  
  # Calculate percent
  loads_percent <-  (text_split / cores) * 100
  
  # Build data frame
  df <- data.frame(
    period = c("one_minute", "five_minute", "fifteen_minute"),
    load = text_split,
    load_percent = loads_percent,
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}
