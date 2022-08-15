#' Function to format a \strong{microbenchmark} return into a tidy tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x A \strong{microbenchmark} object.
#' 
#' @return Tibble. 
#' 
#' @export
format_microbenchmark_return <- function(x) {
  
  # Class check
  stopifnot(inherits(x, "microbenchmark"))
  
  tibble::tibble(
    test = as.character(x$expr),
    seconds = x$time / 1e9
  ) %>% 
    tibble::rowid_to_column("run")
  
}
