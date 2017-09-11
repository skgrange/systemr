#' Function to return a system's extended service set identification (ESSID) 
#' names. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.
#' 
#' @export
system_essid <- function() {
  
  # Get and clean system commands
  suppressWarnings(
    text <- system("iwgetid", intern = TRUE)
  )
  
  # Check return and if nothing return empty data frame
  if (length(text) == 0) {
    
    warning("No valid adaptors detected...", call. = FALSE)
    return(data.frame())
    
  }
  
  text_split <- stringr::str_split(text, " ")[[1]]
  text_split <- text_split[!text_split == ""]
  
  # Clean pieces
  adapter <- text_split[1]
  essid <- text_split[2]
  essid <- stringr::str_split_fixed(essid, ":", 2)[, 2]
  essid <- stringr::str_replace_all(essid, '"', "")
  
  # Build data frame
  df <- data.frame(
    adapter = adapter,
    essid = essid,
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}
