#' Function to return a better formatted \code{Sys.info} call. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
system_info <- function() {
  
  data.frame(
    t(Sys.info()), 
    stringsAsFactors = FALSE
  ) %>% 
    as_tibble()

}
