#' Function to return a formatted version of the Unix \code{last} command. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
system_last_users <- function() {
  
  # Check
  stopifnot(ubuntu_test())
 
  # Get output 
  text <- system("last --time-format iso -xR", intern = TRUE)
  
  # Drop
  text_filter <- grep("shutdown|reboot|runlevel", text, value = TRUE)
  
  # Clean
  text_filter <- stringr::str_replace_all(text_filter, "system boot", "system_boot")
  text_filter <- stringr::str_replace_all(text_filter, "still running", "still_running")
  text_filter <- stringr::str_replace_all(text_filter, "\\s+", " ")
  
  # Only first occurance, stringr fails here
  text_filter <- sub("*\\(.*?\\) *", "to_level", text_filter)
  
  # Split into variables
  text_filter_split <- stringr::str_split_fixed(text_filter, " ", 4)
  
  # # Clean final date
  text_filter_split[, 4] <- stringr::str_split_fixed(
    text_filter_split[, 4], 
    "\\(", 
    2
  )[, 1]
  
  text_filter_split[, 4] <- stringr::str_replace(text_filter_split[, 4], "-", "")
  
  # To data frame
  df <- data.frame(text_filter_split, stringsAsFactors = FALSE)
  
  # Give names
  names(df) <- c("activity", "location", "date_start", "date_end")
  
  # Parse dates, no tz used here
  df$date_start <- lubridate::ymd_hms(df$date_start, quiet = TRUE)
  df$date_end <- lubridate::ymd_hms(df$date_end, quiet = TRUE)
  
  # Transform
  df$time_elapsed_seconds <- as.numeric(df$date_end) - as.numeric(df$date_start)
  df$time_elapsed_hms <- hms::as.hms(df$time_elapsed_seconds)
  df$time_elapsed_hms <- format(df$time_elapsed_hms)
  
  # Arrange
  df <- dplyr::arrange(df, date_start)
  
  # To tibble
  df <- as_tibble(df)
  
  return(df)
  
}
