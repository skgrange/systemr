#' Function to run the \code{speedtest-cli} internet bandwidth programme.
#' 
#' @author Stuart K. Grange
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble.
#' 
#' @export
speed_test <- function(verbose = TRUE) {
  
  # To-do: Test for programme
  
  # Get system date
  date_system <- lubridate::now()
  
  # Run testing programme
  list_speed <- tryCatch({
    processx::run("speedtest", echo = verbose)
  }, error = function(e) {
    NULL
  })
  
  # If fails return here
  if (is.null(list_speed)) return(tibble())
  
  # Format text output
  text_output <- list_speed$stdout %>% 
    stringr::str_split("\n") %>% 
    .[[1]]
  
  # Format the pieces
  text_download <- text_output %>% 
    stringr::str_subset("Download") %>% 
    stringr::str_split_fixed(": | ", 3)
  
  text_upload <- text_output %>% 
    stringr::str_subset("Upload") %>% 
    stringr::str_split_fixed(": | ", 3)
  
  host <- text_output %>% 
    stringr::str_subset("Testing from") %>% 
    stringr::str_remove_all("Testing from |\\.\\.\\.")
  
  server <- stringr::str_subset(text_output, "Hosted by")
  
  # Build tibble
  df <- tibble(
    date = date_system,
    system = hostname(),
    host = host,
    server = server,
    unit = text_download[, 3],
    download = as.numeric(text_download[, 2]),
    upload = as.numeric(text_upload[, 2])
  )
  
  return(df)
  
}
