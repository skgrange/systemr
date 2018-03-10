#' Function to return the system's number of CPU cores. 
#' 
#' @author Stuart K. Grange
#' 
#' @param logical_cores Should logical cores be included in the core count? 
#' 
#' @export
system_cpu_core_count <- function(logical_cores = TRUE) 
  parallel::detectCores(logical = logical_cores)
