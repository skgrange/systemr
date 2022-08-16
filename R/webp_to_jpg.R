#' Function to convert a \code{.webp} image to a \code{.jpg} image. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Vector of file names to be converted. 
#' 
#' @param file_output Optional vector of output file names. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible \code{file}.
#' 
#' @export
webp_to_jpg <- function(file, file_output = NA, verbose = FALSE) {
  
  # Expand file names
  file <- fs::path_expand(file)
  
  # Do the conversion
  purrr::walk2(file, file_output, webp_to_jpg_worker, verbose = verbose)
  
  return(invisible(file))
  
}


webp_to_jpg_worker <- function(file, file_output, verbose) {
  
  # Message to user
  if (verbose) message(threadr::date_message(), "Converting `", file, "`...")
  
  # If no file name is supplied
  if (is.na(file_output)) {
    file_output <- fs::path_ext_set(file, ".jpg")
  }
  
  # Do
  x <- processx::run("convert", args = c(file, file_output))
    
  return(invisible(file_output))
  
}
