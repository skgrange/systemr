#' Function to update images' creation dates. 
#' 
#' @param file Vector of image files.
#' 
#' @param date Vector of dates to update \code{file}.
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Invisible \code{file}. 
#' 
#' @export
image_update_creation_date <- function(file, date, verbose = FALSE, 
                                       progress = FALSE) {
  
  # Check if exiftool is installed
  is_exiftool_installed()
  
  # Check if date is parsed
  stopifnot(lubridate::is.POSIXt(date))
  
  # Do
  x <- purrr::walk2(
    file, date, 
    ~image_update_creation_date_worker(file = .x, date = .y, verbose = verbose),
    .progress = progress
  )
  
  return(invisible(x))
  
}


image_update_creation_date_worker <- function(file, date, verbose) {
  
  # Message to user
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} `{file}`...")
  }
  
  # Format date for metadata
  date_format <- format(date, format = "%Y:%m:%d %H:%M:%OS%z")
  
  # Update date time original
  command_date_time_original <- stringr::str_c("-datetimeoriginal=", date_format)
  command_create_date <- stringr::str_c("-createdate=", date_format)
  
  # Update date time original
  processx::run(
    "exiftool",
    args = c(command_date_time_original, file, "-overwrite_original")
  )

  # Update creation date too
  processx::run(
    "exiftool",
    args = c(command_create_date, file, "-overwrite_original")
  )
  
  return(invisible(file))
  
}


is_exiftool_installed <- function() {
  
  # Check
  is_installed <- processx::run("which", "exiftool") %>% 
    .[["stdout"]] %>% 
    stringr::str_detect("exiftool")
  
  # Error if not installed
  if (!is_installed) {
    cli::cli_abort("`exiftool` not detected.")
  }
  
}
