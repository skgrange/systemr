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
#' @return Invisible.
#' 
#' @export
png_to_jpeg <- function(file, file_output = NA, quality = 95, verbose = FALSE) {
  
  # Check if image magick is installed
  if (!check_image_magick_install()) 
    stop("ImageMagick is not detected...", call. = FALSE)
  
  # Do
  purrr::walk2(
    file,
    file_output,
    png_to_jpeg_worker,
    quality = quality,
    verbose = verbose
  )
  
  # No return

}


png_to_jpeg_worker <- function(file, file_output, quality, verbose) {
  
  # Ensure path is expanded, not really nessassary here
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
  
  # No return
  
}


# An alias
#' @rdname png_to_jpeg
#' 
#' @export
png_to_jpg <- png_to_jpeg
