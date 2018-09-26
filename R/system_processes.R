#' Function to list a system's processes. 
#' 
#' @param by What variable should be used to arrange/sort the returned data 
#' frame? 
#' 
#' @author Stuart K. Grange 
#' 
#' @return Tibble. 
#' 
#' @export
system_processes <- function(by = "cpu") {
  
  # Get all processes
  text <- system("ps -aux", intern = TRUE)
  
  # Get fixed width indices for variables
  text_no_header <- text[-1]
  
  index_pid <- stringr::str_locate(text[1], "PID")[, 1]
  index_cpu <- stringr::str_locate(text[1], "%CPU")[, 1]
  index_memory <- stringr::str_locate(text[1], "%MEM")[, 1]
  index_vsz <- stringr::str_locate(text[1], "VSZ")[, 1]
  
  # index_time <- stringr::str_locate(text[1], "TIME")[, 1]
  index_command <- stringr::str_locate(text[1], "COMMAND")[, 1]
  
  # Get and clean variables
  user <- stringr::str_sub(text_no_header, start = 1, end = index_pid - 3)
  user <- stringr::str_trim(user)
  
  pid <- stringr::str_sub(text_no_header, start = index_pid - 3, end = index_cpu - 1)
  pid <- as.integer(pid)
  
  cpu <- stringr::str_sub(text_no_header, start = index_cpu, end = index_memory -1)
  cpu <- as.numeric(cpu)
  
  memory <- stringr::str_sub(text_no_header, start = index_memory, end = index_memory + 4)
  memory <- as.numeric(memory)
  
  command <- stringr::str_sub(text_no_header, start = index_command)
  command <- stringr::str_trim(command)
  
  # Build data frame
  df <- data_frame(
    user,
    pid, 
    cpu,
    memory,
    command
  )
  
  # Sort the data fame
  if (by == "cpu") {
    
    df <- plyr::arrange(df, plyr::desc(cpu))
    
  } else if (by == "memory") {
    
    df <- plyr::arrange(df, plyr::desc(memory))
    
  } else if (by == "pid") {
    
    df <- plyr::arrange(df, pid)
    
  }
  
  return(df)
  
}
