#' Function to resize images with \code{ImageMagick}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Vector of file names to be resized. 
#' 
#' @param file_output Optional vector of output file names. 
#' 
#' @param quality Quality in pixels. Default is 2000 which is two megapixels. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return Invisible list. 
#' 
#' @export
image_resize <- function(file, file_output = NA, quality = 2000, 
                         verbose = FALSE, progress = FALSE) {
  
  # Check if image magick is installed
  if (!check_image_magick_install()) {
    stop("ImageMagick is not detected.", call. = FALSE)
  }
  
  # Do
  purrr::walk2(
    file, 
    file_output,
    image_resize_worker,
    quality = quality,
    verbose = verbose,
    .progress = progress
  )
  
}


image_resize_worker <- function(file, file_output, quality, verbose) {
  
  # Ensure path is expanded, not really necessary here
  file <- fs::path_expand(file)
  
  # Message to user
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Resizing `{file}`...")
  }
  
  # Build an output file name if not supplied
  if (is.na(file_output))  {
    file_output <- basename(file)
    file_output <- stringr::str_split(file_output, "\\.")[[1]]
    file_suffix <- file_output[length(file_output)]
    file_output <- file_output[-length(file_output)]
    file_output <- stringr::str_c(file_output, collapse = "")
    file_output <- stringr::str_c(file_output, "_resized")
    file_output <- stringr::str_c(file_output, ".", file_suffix)
    file_output <- fs::path(getwd(), file_output)
  }
  
  # Build quality strgin
  quality <- stringr::str_c(quality, "X", quality)
  
  # Do the resizing with image magick
  process <- processx::run(
    "convert", args = c("-resize", quality, file, file_output)
  )
  
  return(invisible(process))
  
}


check_image_magick_install <- function() {
  
  # Get version
  x <- system("convert -version", intern = TRUE)
  
  if (grepl("ImageMagick", x[1])) {
    x <- TRUE
  } else {
    x <- FALSE
  }
  
  return(x)
  
}
