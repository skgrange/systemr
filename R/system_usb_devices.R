#' Function to return a system's USB devices from a \code{lsusb} system call. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.
#' 
#' @export
system_usb_devices <- function() {
  
  # Get system call
  text <- system("lsusb", intern = TRUE)
  
  # Split into variables
  text_split <- stringr::str_split_fixed(text, " ", 7)
  
  # Clean variables
  text_split[, 4] <- stringr::str_replace(text_split[, 4], ":$", "")
  text_split[, 7] <- stringr::str_trim(text_split[, 7])
  
  # Build data frame
  df <- data.frame(
    hostname = hostname(),
    bus = as.integer(text_split[, 2]), 
    device = as.integer(text_split[, 4]), 
    id = text_split[, 6], 
    description = text_split[, 7], 
    stringsAsFactors = FALSE
  )
  
  # Arrange
  df <- plyr::arrange(df, bus, device)
  
  return(df)
  
}
