#' Function to return a formatted \code{Sys.info} call. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
system_info <- function() {
  
  data.frame(
    t(Sys.info()), 
    stringsAsFactors = FALSE
  )

  
}
