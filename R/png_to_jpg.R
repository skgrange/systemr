#' Function to convert a \code{.png} image to a \code{.jpeg} image. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Vector of file names to be resized. 
#' 
#' @param file_output Optional vector of output file names. 
#' 
#' @param quality Quality, represents jpeg compression (100--0). No compression 
#' is 100. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible \code{NULL}.
#' 
#' @export
png_to_jpg <- function(file, file_output = NA, quality = 95, verbose = FALSE) {
  
  # Check if image magick is installed
  if (!check_image_magick_install()) {
    stop("ImageMagick is not detected.", call. = FALSE) 
  }
  
  # Do
  purrr::walk2(
    file,
    file_output,
    png_to_jpg_worker,
    quality = quality,
    verbose = verbose
  )
  
  return(invisible(NULL))

}


png_to_jpg_worker <- function(file, file_output, quality, verbose) {
  
  # Ensure path is expanded, not really necessary here
  file <- path.expand(file)
  
  if (is.na(file_output))  {
    file_output <- basename(file)
    file_output <- stringr::str_split_fixed(file_output, "\\.", 2)[, 1]
    file_output <- stringr::str_c(file_output, ".jpg")
    file_output <- file.path(getwd(), file_output)
  }
  
  # Quote file names
  file <- shQuote(file)
  file_output <- shQuote(file_output)
  
  # Use redirect here
  command <- stringr::str_c(
    "convert -quality ", quality, " ", file, " ", file_output
  )
  
  # Print
  if (verbose) message(threadr::date_message(), file, "...")
  
  # Do
  system(command)
  
  return(invisible(file))
  
}


# An alias
#' @rdname png_to_jpg
#' 
#' @export
png_to_jpeg <- png_to_jpg
