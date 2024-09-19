#' Function to return a somewhat detailed operating system release string. 
#' 
#' @author Stuart K. Grange
#' 
#' @param add_prefix Should a generic prefix (from \code{Sys.info}) be added to 
#' the return? 
#' 
#' @return Character vector with length of \code{1}. 
#' 
#' @export
get_os_release_string <- function(add_prefix = TRUE) {
  
  # Get os name from R
  os_system_name <- stringr::str_to_lower(Sys.info()["sysname"])
  
  if (os_system_name == "windows") {
    
    # Get system info with system call
    df <- get_windows_system_info()
    
    # Build release string
    x <- stringr::str_c(df$value[1], " (", df$value[2], ")")
    
  } else if (os_system_name == "linux") {
    
    # Get release info by reading file
    df <- get_os_release("/etc/os-release")
    
    # Get the string that is desired
    x <- df %>% 
      filter(stringr::str_detect(variable, "(?i)pretty")) %>% 
      pull(value)
    
  }
  
  # Add prefix if desired
  if (add_prefix) {
    x <- stringr::str_c(os_system_name, ":", x)
  }
  
  return(x)
  
}


get_os_release <- function(file = "/etc/os-release") {
  
  if (fs::file_exists(file)) {
    
    # Read release file as text
    text <- readLines(file)
    
    # Format to a tibble
    df <- text %>% 
      stringr::str_remove_all('"') %>% 
      stringr::str_split_fixed("=", n = 2) %>% 
      as_tibble(.name_repair = "minimal") %>% 
      purrr::set_names(c("variable", "value"))
    
  } else {
    df <- tibble()
  }
  
  return(df)
  
}


get_windows_system_info <- function() {
  
  # Run programme, a slow call and `ver` is not available it seems
  list_run <- processx::run("systeminfo")
  
  # Extract and format return
  list_run %>% 
    .[["stdout"]] %>% 
    stringr::str_split_1("\n") %>% 
    stringr::str_subset("^OS Name|^OS Version") %>% 
    stringr::str_squish() %>% 
    stringr::str_split_fixed(": ", n = 2) %>% 
    as_tibble(.name_repair = "minimal") %>% 
    purrr::set_names(c("variable", "value"))
  
}
