#' Function to get system uptime. 
#' 
#' @param round Should the dates and times be rounded to the nearest second? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
system_uptime <- function(round = TRUE) {
  
  # Get time system has been up
  date_system <- lubridate::now()
  
  # Get and clean uptime
  text <- system("cat /proc/uptime", intern = TRUE)
  seconds <- stringr::str_split_fixed(text, " ", 2)[, 1]
  seconds <- as.numeric(seconds)
  
  if (round) {
   
    date_system <- round(date_system)
    seconds <- round(seconds) 
    
  }
  
  # Boot time
  date_up <- date_system - seconds
  
  # Different date types
  date_interval <- lubridate::interval(date_up, date_system)
  date_period <- lubridate::as.period(date_interval)
  date_hms <- hms::as.hms(seconds)
  
  # Format
  date_system <- format(date_system, usetz = TRUE)
  date_up <- format(date_up, usetz = TRUE)
  date_period <- format(date_period)
  date_hms <- format(date_hms)
  
  # Build data frame
  df <- data.frame(
    date_up,
    date_system, 
    seconds_up = seconds,
    period_up = date_period,
    hms_up = date_hms,
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}
