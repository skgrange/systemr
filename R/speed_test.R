#' Function to run the \code{speedtest-cli} internet bandwidth programme.
#' 
#' @author Stuart K. Grange
#' 
#' @param n Number of times to run the speed test. 
#' 
#' @param sleep If \code{n} is not 1, the number of seconds to sleep before 
#' speed tests. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble.
#' 
#' @export
speed_test <- function(n = 1, sleep = NA, verbose = TRUE) {
  
  # If n is 1, no sleep is needed
  if (n == 1 && !is.na(sleep)) {
    sleep <- NA
  }
  
  # Do
  df <- purrr::map_dfr(
    seq_len(n), ~speed_test_worker(sleep = sleep, verbose = verbose)
  )
  
  return(df)
  
}


speed_test_worker <- function(sleep, verbose) {
  
  # TODO: Test for programme
  
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

  # Sleep between runs
  if (!is.na(sleep)) {
    Sys.sleep(sleep)
  }
  
  return(df)
  
}
