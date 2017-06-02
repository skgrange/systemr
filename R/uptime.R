#' Function to get and format the \code{uptime} system call. 
#' 
#' @param round Should the dates and times be rounded to the nearest second? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
uptime <- function(round = FALSE) {
  
  # Get time system has been up
  date_system <- lubridate::now()
  uptime_cat <- system("cat /proc/uptime", intern = TRUE)
  uptime_cat <- stringr::str_split_fixed(uptime_cat, " ", 2)[, 1]
  uptime_cat <- as.numeric(uptime_cat)
  
  if (round) {
   
    date_system <- round(date_system)
    uptime_cat <- round(uptime_cat) 
    
  }
  
  date_up <- date_system - uptime_cat
  
  date_interval <- lubridate::interval(date_up, date_system)
  date_period <- lubridate::as.period(date_interval)
  date_hms <- hms::as.hms(uptime_cat)
  
  # Format things
  date_system <- format(date_system, usetz = TRUE)
  date_up <- format(date_up, usetz = TRUE)
  date_period <- format(date_period)
  
  date_hms <- format(date_hms)
  
  df_times <- data.frame(
    date_up,
    date_system, 
    seconds_up = uptime_cat,
    period_up = date_period,
    hms_up = date_hms
  )
  
  # A system call
  text <- system("uptime", intern = TRUE)
  
  # Split output
  text_split <- stringr::str_split(text, "up|user,|average:")[[1]]
  
  # # Get time
  # time <- stringr::str_trim(text_split[1])
  # time <- hms::as.hms(time)
  # time <- format(time)
  # 
  # # Get uptime
  # uptime_split <- stringr::str_split(text_split[2], ",")[[1]]
  # 
  # # Get pieces
  # days <- threadr::str_filter(uptime_split, "day")
  # hours <- threadr::str_filter(uptime_split, "hour")
  # minutes <- threadr::str_filter(uptime_split, "min")
  # 
  # # Clean
  # days <- length_tester(days)
  # hours <- length_tester(hours)
  # minutes <- length_tester(minutes)
  # 
  # days <- threadr::str_extract_digits(days)
  # hours <- threadr::str_extract_digits(hours)
  # minutes <- threadr::str_extract_digits(minutes)
  # 
  # uptime_seconds <- sum(
  #   days * threadr::seconds_in_a_day(),
  #   hours * threadr::seconds_in_an_hour(),
  #   minutes * 60
  # )
  # 
  # uptime_hms <- hms::as.hms(uptime_seconds)
  # uptime_hms <- format(uptime_hms)
  # 
  # df_uptime <- data.frame(
  #   days,
  #   hours,
  #   minutes,
  #   uptime_seconds,
  #   uptime_hms,
  #   stringsAsFactors = FALSE
  # )
  # 
  # # For zero length vectors
  # length_tester <- function(x) ifelse(length(x) == 0, 0, x)
  
  # Get cores
  cores <- parallel::detectCores()
  
  # Get load averages
  loads <- stringr::str_split_fixed(text_split[4], ", ", 3)[1, ]
  loads <- as.numeric(loads)
  loads_percent <-  (loads / cores) * 100
  
  df_loads <- data.frame(
    period = c("one_minute", "five_minute", "fifteen_minute"),
    load = loads,
    load_percent = loads_percent,
    stringsAsFactors = FALSE
  )
  
  # Build list
  list_uptime <- list(
    date_system = date_system,
    uptime = df_times,
    cores = cores,
    loads = df_loads
  )
  
  return(list_uptime)
  
}
