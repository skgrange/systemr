#' Function to get CPU package temperature. 
#' 
#' The package temperature is the most representative value of CPU temperature
#' as a whole.
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector with length of 1.
#' 
#' @export
get_cpu_package_temperature <- function() {
  
  # Query sensors
  x <- system("sensors", intern = TRUE)
  
  # Get numeric value for the package temperature
  as.numeric(
    sub(
      ".*Package.*:\\s*\\+?([0-9.]+).*", "\\1",
      x[grep("Package", x, ignore.case = TRUE)[1]]
    )
  )
  
}
