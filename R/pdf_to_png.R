#' Function to convert a pdf file to a png file, usually used for single page 
#' pdf images. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Vector of file names. 
#' 
#' @param file_output Optional vector of output file names. 
#' 
#' @param resolution Resolution of png file in DPI. 
#' 
#' @param verbose Should the function give messages?
#' 
#' @return Invisible. 
#' 
#' @export
pdf_to_png <- function(file, file_output = NA, resolution = 320, verbose = FALSE) {
  
  # Check if system programme is installed
  if (!pdftoppm_install_check()) 
    stop("`pdftoppm` system programme is not installed...", call. = FALSE)
  
  # Do
  purrr::walk2(
    file, 
    file_output, 
    pdf_to_png_worker, 
    resolution = resolution, 
    verbose = verbose
  )
  
}
  

pdf_to_png_worker <- function(file, file_output, resolution, verbose) {
  
  # Ensure path is expanded, not really nessassary here
  file <- path.expand(file)
  
  if (is.na(file_output))  {
    
    file_output <- basename(file)
    file_output <- stringr::str_split_fixed(file_output, "\\.", 2)[, 1]
    file_output <- stringr::str_c(file_output, ".png")
    file_output <- file.path(getwd(), file_output)
    
  }
  
  # Quote file names
  file <- shQuote(file)
  file_output <- shQuote(file_output)
  
  # Use redirect here
  command <- stringr::str_c(
    "pdftoppm -rx ", 
    resolution, " -ry ", resolution, " -png ",
    file,
    " > ", 
    file_output
  )
  
  # Print
  if (verbose) message(threadr::date_message(), file, "...")
  
  # Do
  system(command)
  
  # No return
  
}


pdftoppm_install_check <- function() {
  
  # System call
  suppressWarnings(
    x <- system("which pdftoppm", intern = TRUE)
  )
  
  # Test
  x <- if (length(x) == 0) {
    
    x <- FALSE
    
  } else if (grepl("pdftoppm", x, ignore.case = TRUE)) {
    
    x <- TRUE
    
  } else {
    
    x <- FALSE
    
  }
  
  return(x)
  
}
