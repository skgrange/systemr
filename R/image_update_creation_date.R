#' Function to update an image's creation dates. 
#' 
#' @param file File name.
#' 
#' @param date Creation date to update \code{file} with.
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Invisible \code{file}. 
#' 
#' @export
image_update_creation_date <- function(file, date, verbose = FALSE) {
  
  # Check if programme is installed
  detect_exiftool()
  
  # Message to user
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # Check date
  stopifnot(lubridate::is.POSIXt(date))
  
  # Expand path for system calls
  file <- normalizePath(file)
  
  # Format for metadata
  date_string <- format(date, format = "%Y:%m:%d %H:%M:%OS%z")
  
  # Update date time original
  command_date_time_original <- stringr::str_c("-datetimeoriginal=", date_string)
  command_create_date <- stringr::str_c("-createdate=", date_string)
  
  # Update date time original
  processx::run(
    "exiftool",
    args = c(command_date_time_original, file, "-overwrite_original")
  )
  
  # Update creation date
  processx::run(
    "exiftool",
    args = c(command_create_date, file, "-overwrite_original")
  )
  
  return(invisible(file))
  
}


detect_exiftool <- function () {
  text <- suppressWarnings(system("which exiftool", intern = TRUE, 
                                  ignore.stderr = TRUE))
  if (length(text) == 0 || !grepl("exiftool", text)) 
    stop("'exiftool' system programme not detected...", call. = FALSE)
}
