#' Function to copy files with \strong{fs} but file by file to allow for a 
#' progress bar. 
#' 
#' @param path A character vector of paths to copy that currently exist. 
#' 
#' @param path_new A character vector of paths to copy to. 
#' 
#' @param overwrite If a \code{path_new} exists, should is be overwritten? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return Invisible \code{path_new}
#' 
#' @export
file_copy_n <- function(path, path_new, overwrite = FALSE, progress = FALSE) {
  
  # Copy files
  purrr::walk2(
    path, path_new, ~fs::file_copy(.x, .y, overwrite = TRUE), .progress = progress
  )
  
  return(invisible(path_new))
  
}
