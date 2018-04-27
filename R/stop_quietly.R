#' Function to stop an R session without an error. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, the R session will be stopped.
#' 
#' @export
stop_quietly <- function() {
  
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
  
}
