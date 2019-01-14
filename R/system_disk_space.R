#' Function to return a system's file systems' free space information. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
system_disk_space <- function() {
  
  # Get system call
  text <- system("df -h", intern = TRUE)
  text_no_header <- text[-1]
  
  # Get variable locations
  index_size <- stringr::str_locate(text[1], "Size")[, 1]
  index_used <- stringr::str_locate(text[1], "Used")[, 1]
  index_available <- stringr::str_locate(text[1], "Avail")[, 1]
  index_in_use <- stringr::str_locate(text[1], "Use%")[, 1]
  index_mount <- stringr::str_locate(text[1], "Mounted")[, 1]
  
  # Get variables
  file_system <- stringr::str_sub(text_no_header, start = 1, end = index_size - 1)
  size <- stringr::str_sub(text_no_header, start = index_size, end = index_used - 1)
  used <- stringr::str_sub(text_no_header, start = index_used, end = index_available - 1)
  available <- stringr::str_sub(text_no_header, start = index_available, end = index_in_use - 1)
  in_use <- stringr::str_sub(text_no_header, start = index_in_use, end = index_mount - 1)
  mount <- stringr::str_sub(text_no_header, start = index_mount)
  
  # Build data frame
  df <- tibble(
    file_system, 
    size_total = size,
    size_used = used,
    size_available = available,
    in_use,
    mount
  )
  
  # Clean and check data types
  df <- dplyr::mutate_all(df, clean_strings)
  
  return(df)

}


clean_strings <- function(x) {
  
  x <- stringr::str_trim(x)
  x <- type.convert(x, as.is = TRUE)
  return(x)
  
}
