#' Function to return system block devices. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list. 
#' 
#' @export
system_block_devices <- function() {
  
  # Use system programme and return json
  text <- system("lsblk -J", intern = TRUE)
  
  # Parse return
  list_blocks <- jsonlite::fromJSON(text)
  
  # Clean a name
  names(list_blocks$blockdevices) <- ifelse(
    names(list_blocks$blockdevices) == "maj:min", 
    "maj_min",
    names(list_blocks$blockdevices)
  )
  
  return(list_blocks)
  
}
