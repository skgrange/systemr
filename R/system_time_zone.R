#' Function to get system's time zone string. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector with length of one. 
#' 
#' @export
system_time_zone <- function() {
  
  # For ubuntu systems
  x <- tryCatch({
    
    suppressWarnings(
      readLines("/etc/timezone")
    )
    
  }, error = function(e) {
    
    x <- system("grep ZONE /etc/sysconfig/clock", intern = TRUE)
    x <- stringr::str_replace_all(x, '"', "")
    x <- stringr::str_split_fixed(x, "=", 2)[, 2]
    return(x)
    
  })
  
  return(x)
  
}
