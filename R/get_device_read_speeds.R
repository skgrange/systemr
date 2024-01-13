#' Function to run the \code{hdparm} programme to get devices' read speeds, 
#' typically for hard discs. 
#' 
#' \code{get_device_read_speeds} will usually require elevated privileges to 
#' run.
#' 
#' @author Stuart K. Grange
#' 
#' @param device A vector of devices to get read speeds for. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble. 
#' 
#' @export
get_device_read_speeds <- function(device = "/dev/sda1", verbose = FALSE) {
  
  # Run programme for each device
  device %>% 
    purrr::map(~get_device_read_speeds_worker(., verbose = verbose)) %>% 
    purrr::list_rbind()
  
}


get_device_read_speeds_worker <- function(device, verbose) {
  
  # Message to user
  if (verbose) {
    cli::cli_alert_info("Timing `{device}`...")
  }
  
  # Get system's date
  date_system <- lubridate::floor_date(lubridate::now("UTC"), "second")
  
  # Run system programme
  list_run <- processx::run(
    "hdparm", args = c("-Tt", device), error_on_status = FALSE
  )
  
  # If device is missing return an empty tibble
  if (list_run$stdout == "") {
    return(tibble())
  }
  
  # Extract output
  message_run <- list_run %>% 
    .[["stdout"]] %>% 
    stringr::str_squish() %>% 
    stringr::str_split_1(":| = ")
  
  # Extract the speeds
  speed_cached <- stringr::str_subset(message_run, "/sec")[1] %>% 
    stringr::str_split_1(" Timing") %>% 
    .[1]
  
  speed_buffered <- stringr::str_subset(message_run, "/sec")[2]
  
  # Build tibble return
  df <- tibble(
    date = date_system,
    system = hostname(),
    device = message_run[1],
    speed_cached,
    speed_buffered
  )
  
  return(df)
  
}
