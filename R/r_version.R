#' Function to return a formatred \code{R.Version} call. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
r_version <- function() {
  
  # Get version
  df <- data.frame(t(R.Version()))
  
  # Clean names
  names(df) <- stringr::str_replace_all(names(df), "\\.", "_")
  
  # Add some formats too
  df$version_string_short <- stringr::str_c(
    df$language, 
    " ", 
    df$major, 
    ".", 
    df$minor
  )
  
  df$version_string_nickname <- stringr::str_c(
    df$version_string_short,
    " ", 
    df$nickname
  )
  
  return(df)
  
}
