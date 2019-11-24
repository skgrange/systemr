#' Function to synchronize a local and remote directory with \code{rsync}. 
#' 
#' \strong{Please note}, \code{synchronize_directory} will delete files in the 
#' remote directory if not present in the local directory by default. 
#' 
#' @param directory_local Local directory to synchronize to remote directory. 
#' 
#' @param directory_remote Remote directory to synchronize from local directory.
#' 
#' @param calculate_size Should the function calculate and include directory 
#' size in the return? 
#' 
#' @param dry_run Should \code{rsync} be run in dry run mode to simulate, but 
#' not conduct the file synchronization. Good for testing. 
#' 
#' @param delete Should files be deleted? \code{synchronize_directory} will 
#' delete files in the remote directory if not present in the local directory by
#' default. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble with programme timing information.  
#' 
#' @export
synchronize_directory <- function(directory_local, directory_remote, 
                                  calculate_size = TRUE, dry_run = TRUE, 
                                  delete = TRUE, verbose = TRUE) {
  
  # Always verbose when dry_run
  if (dry_run) verbose <- TRUE
  
  # Message to user
  if (verbose) {
    message(
      threadr::date_message(), 
      "Synchronizing `",
      directory_local, 
      "` to `", 
      directory_remote,
      "`..."
    )
  }
  
  # Get system date
  date_system_start <- lubridate::now()
  
  # Build rsync command
  command <- stringr::str_c(
    "rsync -ravh --progress --delete -dryrun", 
    shQuote(directory_local, type = "sh"),
    shQuote(directory_remote, type = "sh"),
    sep = " "
  )
  
  # Drop a few things from command
  if (!verbose) {
    command <- stringr::str_replace(command, "-ravh", "-rah")
    command <- stringr::str_remove(command, "--progress")
  }
  
  # Drop dry run
  if (!dry_run) command <- stringr::str_remove(command, " -dryrun")
  
  # Drop delete
  if (!delete) command <- stringr::str_remove(command, " --delete")
  
  # Clean up command
  command <- stringr::str_squish(command)
  
  # Get size to be synchronized
  if (calculate_size) {
   
    if (verbose) {
      message(threadr::date_message(), "Calculating size of directory...")
    }

    size_directory_local <- system_directory_size(directory_local)$size 
    
  } else {
    size_directory_local <- NA
  }
  
  # Do the synchronisation with rsync
  system(command)
  
  if (dry_run) {
    message(
      threadr::date_message(), 
      "`dry_run` selected, no files synchronized or modified..."
    )
  }
  
  # Get system date
  date_system_end <- lubridate::now()
  
  # Build data frame for return
  df <- tibble(
    system = hostname(),
    task = "synchronize_directory",
    date_start = as.numeric(date_system_start),
    date_end = as.numeric(date_system_end)
  )
  
  # Add the extras
  df$time_elapsed_seconds <- df$date_end - df$date_start
  df$time_elapsed_hms <- str_hms(df$time_elapsed_seconds)
  df$system_load_15_min <- system_load()[3, 3, drop = TRUE]
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
