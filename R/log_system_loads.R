#' Function to log system logs. 
#' 
#' \code{log_system_loads} will take a measurement every five seconds and then
#' aggregate and export the measurements every minute. 
#' 
#' @param directory Directory to export data files too. 
#' 
#' @param json Should the output file be a JSON new lines file? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, data files. 
#' 
#' @importFrom lubridate now floor_date second minute
#' 
#' @export
log_system_loads <- function(directory = NA, json = FALSE) {
  
  # Catch
  if (is.na(directory)) directory <- getwd()
  
  # Check
  stopifnot(file.exists("/proc/loadavg"))
  
  # Set options for programme
  options(digits.secs = 1, digits = 15)
  
  # Get system
  system <- hostname()
  
  # Get number of cores
  cores <- parallel::detectCores()
  
  # Number of seconds
  frequency <- 5
  aggregation_trigger <- 60 / frequency
  
  # Allocate for measurement loop
  list_results <- list()
  
  # Begin measurement loop
  while (TRUE) {
    
    # Nice start for first iteration please
    if (length(list_results) == 0 && floor(second(now())) != 0)
      sleep_until_next_minute()
    
    # Get measurement time
    date_system <- as.numeric(now())
    
    # Get system loads
    results <- get_system_loads(date_system, cores)
    
    # Message
    message(stringr::str_c(results[-2], collapse = ", "))
    
    # Accumulate matrix in list
    list_results <- c(list_results, list(results))
    
    # Aggregation and exporting logic
    if (length(list_results) == aggregation_trigger) {
      
      message("Aggregating and exporting data...")
      
      # Bind objects within list
      matrix_results <- do.call("rbind", list_results)
      
      # Get dates
      date_system_first <- matrix_results[1, 1]
      
      # Make well formated human readable dates
      date <- as.POSIXct(date_system_first, tz = "UTC", origin = "1970-01-01")
      date_string <- format.POSIXct(date, format = "%Y-%m-%d %H:%M:%OS", usetz = TRUE)
      
      # Aggregate, mean
      vector_results <- apply(matrix_results[, -1:-2], 2, mean)
      vector_results <- round(vector_results, 1)
      
      # Add extras
      vector_results <- c(system, date_system_first, date_string, vector_results)
      
      # Give names
      names(vector_results) <- c(
        "system", "date_unix", "date", "cpu_load", "memory_load"
      )
      
      # To data frame
      df <- data.frame(t(vector_results), stringsAsFactors = FALSE)
      
      if (json) {
        
        file_name <- stringr::str_c(system, "_system_loads.jsonl")
        file_name <- file.path(directory, file_name)
        
        # Write
        threadr::write_json_lines(df, file_name, append = TRUE)
        
      } else {
        
        # Build file name
        day <- stringr::str_split_fixed(date_string, " ", 2)[, 1]
        file_name <- stringr::str_c(day, "_", system, "_system_loads.csv")
        file_name <- file.path(directory, file_name)
        
        # Write to file
        if (file.exists(file_name)) {
          
          readr::write_csv(df, file_name, append = TRUE)
          
        } else {
          
          message("Creating file...")
          readr::write_csv(df, file_name, append = FALSE)
          
        }
        
      }
      
      # Resign to empty list for next period
      list_results <- list()
      
      if (minute(date) %% 15 == 0) {
        
        message("Housekeeping...")
        gc()
        
      }
      
    }
    
    sleep_until_next_clean_time(frequency)
    
  }
  
  # No return
  
}


get_system_loads <- function(date, cores) {
  
  # Get system load
  x <- readLines("/proc/loadavg")
  x <- stringr::str_split(x, " ")[[1]][1]
  x <- as.numeric(x) / cores * 100
  
  # Get memory usage
  y <- system_memory_usage()
  
  matrix <- matrix(c(date, NA, x, y), nrow = 1, ncol = 4)
  
  return(matrix)
  
}


sleep_until_next_clean_time <- function(seconds) {
  
  # Now
  date <- as.numeric(now())
  
  # Next loop? 
  date_next <- floor(date) + seconds
  
  # How long to wait? 
  seconds_to_wait <- date_next - date

  # Ensure date is nice and clean, if not fix it
  seconds_modulo <- date_next %% seconds
  
  if (!seconds_modulo == 0) 
    date_next <- date_next - seconds_modulo
  
  # Overwrite if needed
  seconds_to_wait <- date_next - as.numeric(now())
  
  Sys.sleep(seconds_to_wait)
  
}


sleep_until_next_minute <- function(verbose = TRUE) {
  
  # Get date
  date <- Sys.time()
  
  # Floor round
  date_floor <- floor_date(date, "minute")
  
  # How many seconds to wait for? 
  seconds_to_wait <- as.numeric(date) - as.numeric(date_floor)
  seconds_to_wait <- 60 - seconds_to_wait
  
  # Print a message
  if (verbose) {
    
    message(
      stringr::str_c(
        "Waiting ", 
        round(seconds_to_wait), 
        " seconds until beginning of next minute..."
      )
    )
    
  }
  
  # Sleep until next minute
  Sys.sleep(seconds_to_wait)
  
}
