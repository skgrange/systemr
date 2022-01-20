#' Function to return the system's number of CPU cores. 
#' 
#' @author Stuart K. Grange
#' 
#' @param logical_cores Should logical cores be included in the core count? 
#' 
#' @param max_cores Should the return have a maximum value? This can be useful 
#' when there are very many cores and logic is being built. 
#' 
#' @return Integer vector with length of 1. 
#' 
#' @examples 
#' 
#' # Find core count
#' system_cpu_core_count()
#' 
#' @export
system_cpu_core_count <- function(logical_cores = TRUE, max_cores = NA) {
  x <- as.integer(parallel::detectCores(logical = logical_cores))
  if (!is.na(max_cores)) {
    max_cores <- as.integer(max_cores)
    x <- if_else(x >= max_cores, max_cores, x) 
  }
  return(x)
}
