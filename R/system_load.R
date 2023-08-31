#' Function to return formatted system loads.
#' 
#' @author Stuart K. Grange
#' 
#' @param as_vector Should a numeric vector be returned rather than a tibble? 
#' 
#' @return Tibble or when \code{as_vector} is \code{TRUE}, a numeric vector of
#' # loads in percent. 
#' 
#' @examples
#' 
#' # Return a tibble
#' system_load()
#' 
#' # Return a numeric vector, in percent
#' system_load(as_vector = TRUE)
#' 
#' @export
system_load <- function(as_vector = FALSE) {
  
  # Read load averages from system
  loads_text <- readLines("/proc/loadavg", n = 1)
  
  # Make a numeric vector
  loads_numeric <- loads_text %>% 
    stringr::str_split_1(" ") %>% 
    .[1:3] %>% 
    as.numeric()
  
  # Get cpu core count
  n_cores <- parallel::detectCores()
  
  # Calculate percent
  loads_percent <-  (loads_numeric / n_cores) * 100
  
  # Two different returns
  if (as_vector) {
    
    # Only the loads in percent
    return(loads_percent)
    
  } else {
    
    # Build tibble
    df <- tibble(
      period = c("one_minute", "five_minute", "fifteen_minute"),
      load = loads_numeric,
      load_percent = loads_percent
    )
    
    return(df)
    
  }

}
