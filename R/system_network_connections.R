#' Function to list a system's network connections. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
system_network_connections <- function() {
  
  # Run and capture system call
  text <- system("nmcli con", intern = TRUE)
  text_no_header <- text[-1]
  
  # Get variable indices
  index_uuid <- stringr::str_locate(text[1], "UUID")[, 1]
  index_type <- stringr::str_locate(text[1], "TYPE")[, 1]
  index_device <- stringr::str_locate(text[1], "DEVICE")[, 1]
  
  # Clean
  name <- stringr::str_sub(text_no_header, start = 1, end = index_uuid - 1)
  name <- stringr::str_trim(name)
  
  uuid <- stringr::str_sub(text_no_header, start = index_uuid, end = index_type - 1)
  uuid <- stringr::str_trim(uuid)
  
  type <- stringr::str_sub(text_no_header, start = index_type, end = index_device - 1)
  type <- stringr::str_trim(type)
  
  device <- stringr::str_sub(text_no_header, start = index_device)
  device <- stringr::str_trim(device)
  device <- ifelse(device == "--", NA, device)
  
  # Build data frame
  df <- tibble(name, uuid, type, device)
  
  return(df)
  
}
