#' Function to convert a  \code{svg} file to a \code{pdf} file. 
#' 
#' \code{svg_to_pdf} requires inkscape to be installed. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Vector of file names. 
#' 
#' @param file_output An optional vector of output file names. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible list containing \code{file}. 
#' 
#' @export
svg_to_pdf <- function(file, file_output = NA, verbose = FALSE) {
  
  # Check if inkscape is installed
  stopifnot(is_inkscape_installed())
  
  # Make sure file vector is expanded
  file <- fs::path_expand(file)
  
  # Convert svg to pdf file
  purrr::walk2(file, file_output, svg_to_pdf_worker, verbose = verbose)
  
}


svg_to_pdf_worker <- function(file, file_output, verbose) {
  
  # Message to user
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # If output file is not supplied
  if (is.na(file_output[1])) {
    file_output <- fs::path_ext_set(file, ".pdf")
  }
  
  # Build system command
  file_output_command <- stringr::str_c("--export-pdf=", file_output)
  
  # Call inkscape
  x <- processx::run("inkscape", args = c(file, file_output_command))
  
  return(invisible(x))
  
}


is_inkscape_installed <- function() {
  
  tryCatch({
    x <- processx::run("which", args = "inkscape")
    TRUE
  }, error = function(e) {
    FALSE
  })
  
}
