#' Function to convert raw image files (usually \code{CR2}) to \code{jpg} image
#' files. 
#' 
#' \code{image_raw_to_jpg} requires \code{darktable-cli} to be installed. 
#' 
#' @param file Vector of raw image file names to be converted. 
#' 
#' @param file_output If desired, a vector of output file names. If not used, 
#' \code{file} will be adapted.  
#' 
#' @param format Output format of the image file. Only \code{jpg} is supported 
#' at this time. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \url{https://www.darktable.org/usermanual/en/special-topics/program-invocation/darktable-cli}
#' 
#' @return Invisible list. 
#' 
#' @export
image_raw_to_jpg <- function(file, file_output = NA, format = "jpg", 
                             verbose = FALSE) {
  
  # Check if system programme is installed
  stopifnot(stringr::str_detect(detect_darktable_cli(), "darktable-cli"))
  stopifnot(format == "jpg")
  
  # Parse input
  file <- fs::path_expand(file)
  
  # Build output file names
  if (is.na(file_output[1])) {
    file_output <- file %>% 
      fs::path_ext_remove() %>% 
      stringr::str_c("_converted_from_raw") %>% 
      fs::path(ext = "jpg")
  } else {
    file_output <- fs::path_expand(file_output)
  }
  
  # Check
  if (any(fs::file_exists(file_output))) {
    stop(
      "Output files exist, this programme will not overwrite these files...",
      call. = FALSE
    )
  }
  
  # Do
  x <- purrr::walk2(
    file, 
    file_output, 
    image_raw_to_jpg_worker, 
    format = format, 
    verbose = verbose
  )
  
  return(invisible(x))
  
}


image_raw_to_jpg_worker <- function(file, file_output, format, verbose) {
  
  # Message to user
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # Use darktable to convert the image
  x <- processx::run(
    "darktable-cli", args = c(file, file_output), echo_cmd = FALSE, echo = FALSE
  )
  
  return(invisible(x))
  
}


detect_darktable_cli <- function() {
  processx::run(
    "which", args = "darktable-cli"
  )$stdout
}
