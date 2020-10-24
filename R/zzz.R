#' Function to squash R check's global variable notes. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "variable", "date_start", "value", "bus", "device", "language", "major", 
    "minor", "version_string_short", "nickname"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
