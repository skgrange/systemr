#' Function to return a system's media access control (MAC) addresses. 
#' 
#' @param format Should the MAC addresses be formated to be upper case and 
#' contain hyphens as the separator? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.
#' 
#' @export
system_mac_addresses <- function(format = FALSE) {
  
  # Get and clean system call
  text <- system("ifconfig", intern = TRUE)
  text_filter <- threadr::str_filter(text, "HWaddr")
  text_filter <- stringr::str_squish(text_filter)
  text_filter <- stringr::str_trim(text_filter)
  
  # Split into variables
  list_text_filter <- stringr::str_split(text_filter, " ")
  
  # Extract the important things
  adapter <- sapply(list_text_filter, "[[", 1)
  mac_address <- sapply(list_text_filter, function(x) tail(x, 1))
  
  if (format) {
    
    mac_address <- stringr::str_to_upper(mac_address)
    mac_address <- stringr::str_replace_all(mac_address, ":", "-")
    
  }
  
  # Build data frame
  df <- data.frame(
    adapter = adapter,
    mac_address = mac_address,
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}
