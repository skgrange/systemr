#' Function to crop a pdf file. 
#' 
#' \code{pdf_crop} requires the PDFCROP system tool to be installed. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Vector of file names. 
#' 
#' @param file_output Optional vector of output file names. 
#' 
#' @return Invisible list. 
#' 
#' @export
pdf_crop <- function(file, file_output = NA) {
  
  purrr::walk2(
    file,
    file_output,
    pdf_crop_worker
  )
  
}


pdf_crop_worker <- function(file, file_output) {
  
  # Ensure path is expanded 
  file <- fs::path_expand(file)
  
  # Make file name if not given
  if (is.na(file_output))  {
    file_output <- file %>% 
      fs::path() %>% 
      fs::path_ext_remove() %>% 
      stringr::str_c("_cropped.pdf")
  } else {
    file_output <- fs::path_expand(file_output)
  }
  
  # Do
  system_call <- processx::run("pdfcrop", args = c(file, file_output), echo = FALSE)
  
  return(invisible(system_call))
  
}
