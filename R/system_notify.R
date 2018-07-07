#' Function to send a system notification to the Ubuntu desktop.
#' 
#' @param title Title for notification. 
#' 
#' @param message Message for notification.
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible.
#' 
#' @export
system_notify <- function(title = "R", message = "") {
  
  # Check
  stopifnot(ubuntu_test())
  
  # Build command
  title <- stringr::str_c('"', title, '"')
  message <- stringr::str_c('"', message, '"')
  x <- stringr::str_c("notify-send -u critical", title, message, sep = " ")
  
  # System call
  system(x)
  
}
