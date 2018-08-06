#' Function to send a system notification to the Ubuntu desktop.
#' 
#' @param urgency Urgency of message, one of \code{"low"}, \code{"normal"}, or
#' \code{"critical"}. Default is \code{"critical"} because it is the most 
#' supported notification type. 
#' 
#' @param title Title for notification. 
#' 
#' @param message Message for notification.
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, a desktop notification. 
#' 
#' @export
system_notify <- function(urgency = "critical", title = "R: ", message = "") {
  
  # Check
  stopifnot(ubuntu_test())
  stopifnot(urgency %in% c("low", "normal", "critical"))
  
  # Build command pieces
  command_notify <- stringr::str_c("notify-send -u ", urgency)
  title <- stringr::str_c('"', title, '"')
  message <- stringr::str_c('"', message, '"')
  
  # Combine
  command_notify <- stringr::str_c(command_notify, title, message, sep = " ")
  
  # System call
  system(command_notify)
  
}
