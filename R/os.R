#' Function to get operating system type.
#' 
#' @author Stuart K. Grange
#' 
#' @export
os <- function() as.character(.Platform$OS.type)
