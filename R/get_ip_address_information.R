#' Function to get public IP address information from the 
#' \url{http://ip-api.com} API. 
#' 
#' @author Stuart K. Grange
#' 
#' @param as_list Should a list be returned with no manipulation -- the data 
#' that the API returns. 
#' 
#' @return Tibble or named list. 
#' 
#' @examples
#' 
#' # Get ip address information
#' get_ip_address_information()
#' 
#' @export
get_ip_address_information <- function(as_list = FALSE) {
  
  # Get current date to nearest second
  date_system <- lubridate::round_date(lubridate::now())
  
  # The url request
  url <- "http://ip-api.com/json/?fields=status,message,continent,continentCode,country,countryCode,region,regionName,city,district,zip,lat,lon,timezone,offset,currency,isp,org,as,asname,reverse,mobile,proxy,hosting,query"
  
  # Get data from the api
  response <- get_possibly(url)
  
  # If an empty return
  if (length(response) == 0L && as_list) {
    return(response)
  } 
  
  if (length(response) == 0L && !as_list) {
    return(tibble())
  }
  
  # Extract the parsed (json) content of the request
  list_ip_address <- httr::content(response)
  
  # Return the naked list if desired
  if (as_list) {
    return(list_ip_address)
  }
  
  # To tibble with some formatting choices
  df <- list_ip_address %>% 
    as_tibble() %>% 
    dplyr::rename_with(threadr::str_to_underscore) %>% 
    select(-status) %>% 
    mutate(
      dplyr::across(
        tidyselect::vars_select_helpers$where(is.character), 
        ~if_else(. == "", NA_character_, .)
      ),
      date = !!date_system,
      hostname = !!hostname()
    ) %>% 
    relocate(hostname,
             date,
             query) %>% 
    rename(ip_address = query, 
           country = country,
           country_iso_code = country_code,
           postcode = zip,
           latitude = lat, 
           longitude = lon,
           time_zone = timezone)
  
  return(df)
  
}


get_possibly <- purrr::possibly(httr::GET, otherwise = list())
