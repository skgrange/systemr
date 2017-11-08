#' Function to synchronize a local and remote directory with \code{rsync}. 
#' 
#' \code{synchronize_directory} will delete files in the remote directory if not 
#' present in the local directory. 
#' 
#' @param directory_local Local directory to synchronize to remote directory. 
#' 
#' @param directory_remote Remote directory to synchronize from local directory.
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame with programme timing information.  
#' 
#' @export
synchronize_directory <- function(directory_local, directory_remote, 
                                  verbose = TRUE) {
  
  # Get system date
  date_system_start <- lubridate::now()
  
  # Build rsync command
  command <- stringr::str_c(
    "rsync -ravh --progress --delete", 
    directory_local,
    directory_remote, 
    sep = " "
  )
  
  # Drop a few things from command
  if (!verbose) {
    
    command <- stringr::str_replace(command, "-ravh", "-rah")
    command <- stringr::str_replace(command, "--progress", "")
    
  }
  
  # Clean up command
  command <- str_trim_many_spaces(command)
  
  # Get size to be synchronized
  size_directory_local <- system_directory_size(directory_local)$size
  
  # Do the synchronisation
  system(command)
  
  # Get system date
  date_system_end <- lubridate::now()
  
  # Build data frame for return
  df <- data.frame(
    system = hostname(),
    task = "synchronize_directory",
    date_start = as.numeric(date_system_start),
    date_end = as.numeric(date_system_end),
    stringsAsFactors = FALSE
  )
  
  # Add the extras
  df$time_elapsed_seconds <- df$date_end - df$date_start
  df$time_elapsed_hms <- str_hms(df$time_elapsed_seconds)
  df$system_load_15_min <- system_load()[3, 3]
  df$size_directory_local <- size_directory_local
  df$command <- command
  
  return(df)
  
}


# From threadr
str_hms <- function (x, round = NA) {
  stopifnot(any(c("hms", "numeric", "integer") %in% class(x)))
  if (any(c("numeric", "integer") %in% class(x))) 
    x <- hms::as.hms(x)
  if (!is.na(round)) {
    x <- round(x, round)
    x <- hms::as.hms(x)
  }
  x <- format(x)
  x <- stringr::str_trim(x)
  x <- ifelse(x == "NA", NA, x)
  return(x)
}


str_trim_many_spaces <- function(x) stringr::str_replace_all(x, "\\s+", " ")