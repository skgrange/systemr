#' Function to format \code{sensors} output. 
#' 
#' @return Data frame.
#' 
#' @author Stuart K. Grange
#' 
#' @export
system_sensors <- function() {
  
  # Get system things, not used at the moment
  date_system <- lubridate::now()
  
  # What variables to get? 
  variables <- c(
    "processor fan", "video fan", "ambient", "core 0", "core 1", "core 2", 
    "core 3", "temp1"
  )
  
  # System call
  text <- tryCatch({
    
    system("sensors", intern = TRUE, ignore.stderr = TRUE)
    
  }, error = function(e) {
    
    character()
    
  })
  
  # acpitz-virtual-0 is the temperature sensor near/on your CPU socket. This sensor can be unreliable.
  # coretemp-isa-0000 measures the temperature of the specific cores.
  # dell_smm-virtual-0 is your CPU fan, managed by your system firmware.
  
  if (length(text) != 0) {
    
    # Clean board infomation 
    index <- tail(grep("Adapter", text), 1)
    cpu_text <- text[(index + 1):length(text)]
    
    value_cpu <- sapply(c("Processor", "Video", "CPU"), function(x) 
      format_value(cpu_text, x), USE.NAMES = FALSE)
    
    names(value_cpu) <- c("cpu_fan_rpm", "video_fan_rpm", "cpu_temperature")
    
    df_cpu <- data.frame(
      t(value_cpu), 
      stringsAsFactors = FALSE
    )
    
    # Get core temperatures too
    text_cores <- threadr::str_filter(text, "Core ")
    value_cores <- str_extract_the_value(text_cores)
    
    # Build sensors
    list_sensors <- list(
      board = df_cpu, 
      cpu_core_temperatures = value_cores
    )
    
  } else {
    
    # Just an empty list
    list_sensors <- list()
    
  }
  
  return(list_sensors)
  
}


str_extract_the_value <- function(x) {
  
  x <- stringr::str_split_fixed(x, ":|\\(", 3)[, 2]
  x <- stringr::str_replace_all(x, "[^0-9.]", "")
  x <- as.numeric(x)
  return(x)
  
}


format_value <- function(x, name, ignore.case = TRUE) {
  
  x <- threadr::str_filter(x, name, ignore.case = ignore.case)[1]
  x <- str_extract_the_value(x)
  name <- stringr::str_to_lower(name)
  name <- stringr::str_replace_all(name, " ", "_")
  names(x) <- name
  return(x)
  
}
