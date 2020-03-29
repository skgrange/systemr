#' Function to convert raw image files (usually \code{CR2}) to \code{jpg} image
#' files. 
#' 
#' \code{image_raw_to_jpg} requires \code{ufraw-batch} to be installed. 
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
#' @seealso \url{https://helpmanual.io/man1/ufraw-batch/}
#' 
#' @return Invisible list. 
#' 
#' @export
image_raw_to_jpg <- function(file, file_output = NA, format = "jpg", 
                             verbose = FALSE) {
  
  # Check for programme
  stopifnot(stringr::str_detect(detect_ufraw_batch(), "ufraw-batch"))
  
  stopifnot(format == "jpg")
  
  # Parse input
  file <- fs::path_expand(file)
  
  # 
  if (is.na(file_output[1])) {
    file_output <- file %>% 
      fs::path_ext_remove() %>% 
      stringr::str_c("_converted") %>% 
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
  
  # 
  command_output <- stringr::str_c("--output=", file_output)
  
  # Do
  x <- tryCatch({
    processx::run(
      "ufraw-batch", args = c("--out-type", format, file, command_output),
      echo_cmd = FALSE,
      echo = FALSE
    )
  }, error = function(e) {
    NULL
  })
  
  return(invisible(x))
  
}


detect_ufraw_batch <- function() {
  processx::run(
    "which", args = "ufraw-batch"
  )$stdout
}
