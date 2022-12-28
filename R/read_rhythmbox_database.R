#' Functions to read XML files created by the Rhythmbox music library software.
#' 
#' @author Stuart K. Grange
#' 
#' @param file File name of Rhythmbox's XML database or Rhythmbox's playlist 
#' database. 
#' 
#' @return Tibble, nested in the case of \code{read_rhythmbox_database} and
#' standard by \code{format_rhythmbox_playlist_entry}.
#' 
#' @export
read_rhythmbox_database <- function(file) {
  
  # Read xml as a list, pretty slow
  list_xml <- threadr::read_xml(file)
  
  # To a single table
  df <- list_xml %>%
    purrr::map(purrr::compact) %>%
    purrr::map_dfr(as_tibble)
  
  # Back to a named list
  list_db <- df %>% 
    dplyr::group_split(.attrs) %>%
    purrr::map(threadr::drop_na_columns) %>% 
    purrr::set_names(c("files", "iradio", "songs", "version")) %>% 
    purrr::map(
      ~dplyr::rename_with(., ~stringr::str_replace_all(., "-", "_")),
      ~dplyr::rename_with(., ~stringr::str_remove_all(., "\\."))
    )
  
  # Clean some variables
  list_db$version <- as.numeric(list_db$version)
  
  list_db$songs <- list_db$songs %>% 
    select(-.attrs) %>% 
    mutate(location = threadr::str_url_decode(location),
           location = stringr::str_remove(location, "^file://"),
           mountpoint = threadr::str_url_decode(mountpoint),
           mountpoint = stringr::str_remove(mountpoint, "^file://"))
  
  list_db$files <- list_db$files %>% 
    select(-.attrs) %>% 
    mutate(location = threadr::str_url_decode(location),
           location = stringr::str_remove(location, "^file://"),
           mountpoint = threadr::str_url_decode(mountpoint),
           mountpoint = stringr::str_remove(mountpoint, "^file://"))
  
  list_db$iradio <- select(list_db$iradio, -.attrs)
  
  # To a nested tibble
  df_nest <- list_db %>% 
    t() %>% 
    as_tibble() %>% 
    rowwise()
  
  return(df_nest)
  
}


#' @rdname read_rhythmbox_database
#' @export
read_rhythmbox_playlists <- function(file) {
  
  # Read xml as a list
  list_xml <- threadr::read_xml(file)
  
  list_xml %>% 
    purrr::map_dfr(format_rhythmbox_playlist_entry, .id = "playlist") %>% 
    mutate(playlist = threadr::str_extract_digits(playlist))
  
}


format_rhythmbox_playlist_entry <- function(l) {
  
  # Get attributes
  if (inherits(l, "character")) {
    attributes <- l
  } else {
    attributes <-  l$.attrs
  }
  
  # Clean
  df <- attributes %>%
    t() %>% 
    data.frame() %>% 
    as_tibble() %>% 
    dplyr::rename_with(., threadr::str_to_underscore)
  
  # Get files
  list_files <- l[-length(l)]
  
  if (all(unique(names(list_files)) == "location")) {
    df_files <- tibble(file = as.character(list_files))
  } else {
    df_files <- tibble(file = NA_character_)
  }
  
  # Bind sets
  df <- dplyr::bind_cols(df, df_files)
  
  return(df)
  
}
