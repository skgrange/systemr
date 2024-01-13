#' Function to get a system's mount point and device information. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @examples
#' 
#' \dontrun{
#' # Get mount information
#' get_find_mount_data()
#' }
#' 
#' 
#' @export
get_find_mount_data <- function() {
  
  # Get system date
  date_system <- lubridate::floor_date(lubridate::now(tz = "UTC"), "second")
  
  # Get system
  system <- hostname()
  
  # Run system programme
  list_run <- processx::run("findmnt", args = "-Ar")
  
  # Get and clean return
  list_text <- list_run %>% 
    .[["stdout"]] %>% 
    stringr::str_split_1("\n") %>% 
    .[-1] %>% 
    utils::head(-1) %>% 
    stringr::str_split(" ") 
  
  # Format each entry, make a tibble, and add a few other things to the return
  df <- list_text %>% 
    purrr::map(format_find_mount_text) %>% 
    purrr::map(
      ~purrr::set_names(., c("mount", "device", "file_system", "options"))
    ) %>% 
    purrr::list_rbind() %>% 
    mutate(date = !!date_system,
           system = !!system) %>% 
    relocate(date,
             system)
  
  return(df)
  
}


format_find_mount_text <- function(x) {
  
  # Message suppression is for name creation
  x %>% 
    t() %>% 
    as_tibble(.name_repair = "minimal")
  
}
