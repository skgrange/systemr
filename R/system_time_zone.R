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
    
    # Get information, watch escape of \
    x <- system("grep 'TIMEZONE\\|ZONE' /etc/sysconfig/clock", intern = TRUE)
    
    # Drop default if exists
    x <- grep("DEFAULT", x, invert = TRUE, value = TRUE)
    
    # Clean
    x <- stringr::str_replace_all(x, '"', "")
    x <- stringr::str_split_fixed(x, "=", 2)[, 2]
    
    return(x)
    
  })
  
  return(x)
  
}
