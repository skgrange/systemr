#' Function to get time zone information from a date, latitude, and longitude 
#' triad from \url{https://timezonedb.com/api}.
#' 
#' To use this function an API key is needed from \url{https://timezonedb.com}.
#' 
#' @author Stuart K. Grange
#' 
#' @param list List or data frame containing at least \code{date}, 
#' \code{latitude}, and \code{longitude} elements or variables. 
#' 
#' @param key \code{timezonedb.com} API key. 
#' 
#' @param sleep Number of seconds to sleep between API queries. One second 
#' between queries is needed when a non-paid version of the API is being used. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @param df Data frame/tibble return from \code{get_time_zone}. 
#' 
#' @return Tibble. 
#' 
#' @export
get_time_zone <- function(list, key, sleep = 1, progress = FALSE,
                          verbose = FALSE) {
  
  # Vectorise function, keep the id variable here for joining
  purrr::pmap(
    list, 
    get_time_zone_worker, 
    key = key, 
    sleep = sleep, 
    verbose = verbose,
    .progress = progress
  ) %>% 
    purrr::list_rbind()
  
}


get_time_zone_worker <- function(id, date, latitude, longitude, key, 
                                 sleep, verbose, ...) {
  
  # Check date
  stopifnot(lubridate::is.POSIXt(date) | is.numeric(date))
  
  # Build url for api
  url <- stringr::str_c(
    "http://api.timezonedb.com/v2.1/get-time-zone?key=",
    key, 
    "&format=json&by=position&lat=",
    latitude,
    "&lng=",
    longitude,
    "&time=",
    as.numeric(date)
  )
  
  # Message to user
  if (verbose) {
    url_no_key <- stringr::str_replace(url, key, "***")
    message(threadr::date_message(), "`", url_no_key, "`...")
  }
  
  # Get and parse json
  df <- url %>% 
    readr::read_lines(progress = FALSE) %>% 
    threadr::read_json() %>% 
    purrr::compact() %>% 
    as_tibble() %>% 
    mutate(id = !!id) %>% 
    relocate(id)
  
  # Sleep to keep the server happy
  Sys.sleep(sleep)
  
  return(df)
  
}


#' @rdname get_time_zone
#' @export
clean_time_zone_table <- function(df) {
  
  df %>% 
    mutate(date_api = threadr::parse_unix_time(timestamp, tz = "UTC"),
           daylight_savings = as.logical(as.integer(dst))) %>% 
    select(id,
           date_api, 
           country_iso_code = countryCode,
           country_name = countryName,
           time_zone = zoneName,
           time_zone_abbreviation = abbreviation,
           utc_offset = gmtOffset,
           daylight_savings)
  
}
