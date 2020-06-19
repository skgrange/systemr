#' Function to combine \code{pdf} files using \code{pdftk}.
#' 
#' @author Stuart K. Grange
#' 
#' @param file Vector of file names to be combined. If not used, all files in 
#' working directory will be combined. 
#' 
#' @param file_output Output file name. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible. 
#' 
#' @export
pdf_combine <- function(file = NA, file_output = "combined_file.pdf",
                        verbose = FALSE) {
  
  # Parse output
  file_output <- fs::path_expand(file_output)
  file_output <- shQuote(file_output)
  
  # Build commands
  if (!is.na(file[1])) {
    file <- shQuote(file)
    file <- stringr::str_c(file, collapse = " ")
    command <- stringr::str_c("pdftk ", file, " cat output ", file_output)
  } else {
    command <- stringr::str_c("pdftk *.pdf cat output ", file_output)
  }
  
  # Run command
  if (verbose) message(threadr::date_message(), command)
  system(command)
  
  return(invisible(command))
  
}
