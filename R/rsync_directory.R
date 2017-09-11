#' Function to synchronize a local and remote directory with \code{rsync}. 
#' 
#' \code{rsync_directory} will delete files in the remote directory if not 
#' present in the local directory. 
#' 
#' @param directory_local Local directory to synchronize to remote directory. 
#' 
#' @param directory_remote Remote directory to synchronize from local directory.
#' 
#' @param file_database Optional, database file. See \code{\link{db_connect}}. 
#' 
#' @param database Optional, database within \code{file_database}. 
#' See \code{\link{db_connect}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible. 
#' 
#' @export
rsync_directory <- function(directory_local, directory_remote, 
                            file_database = NA, database = NA) {
  
  # Get system date
  date_system <- lubridate::now()
  
  # Build rsync command
  command <- stringr::str_c(
    "rsync -ravh --progress --delete", 
    directory_local,
    directory_remote, 
    sep = " "
  )
  
  # Do 
  system(command)
  
  if (!is.na(file_database)) {
    
    # Get post command date
    date_system_end <- lubridate::now()
    
    # Calculate run duration
    run_duration <- date_system_end - date_system
    run_duration <- hms::as.hms(run_duration)
    run_duration <- threadr::str_hms(run_duration, 2)
    
    # Build infomation data frame
    df <- data.frame(
      date_system = as.numeric(date_system),
      date_system_end = as.numeric(date_system_end),
      duration = run_duration,
      user = system_info()$user,
      hostname = hostname(),
      os_description = ubuntu_release()$description,
      command,
      stringsAsFactors = FALSE
    )
    
    # Insert into database
    databaser::db_direct_insert(
      file_database, 
      database = database, 
      table = "back_up_information", 
      df = df, 
      replace = FALSE
    )
    
  }
  
  # No return
  
}
