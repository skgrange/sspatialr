#' Function to transform a \strong{sf} spatial object to a data frame/tibble.
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} spatial object. 
#' 
#' @return Tibble. 
#' 
#' @export
sf_to_df <- function(sf) {
  
  # Keep non-spatial variables
  df_non_spatial <- sf::st_drop_geometry(sf) %>% 
    tibble::rowid_to_column("rowid")
  
  # Extract coordinates
  df_coordinates <- sf %>% 
    sf::st_coordinates() %>% 
    as_tibble()
  
  if (ncol(df_coordinates) == 2L) {
    df_coordinates <- purrr::set_names(df_coordinates, c("x", "y"))
  } else if (ncol(df_coordinates) == 3L) {
    df_coordinates <- purrr::set_names(df_coordinates, c("x", "y", "rowid"))
  } else {
    stop("Too many columns.", call. = FALSE)
  }
  
  if (ncol(df_coordinates) == 2L) {
    # Bind the two types of variables
    df <- dplyr::bind_cols(df_non_spatial, df_coordinates)  
  } else if (ncol(df_coordinates) == 3L) {
    df <- df_coordinates %>% 
      left_join(df_non_spatial, by = join_by(rowid)) %>% 
      select(-rowid)
  }
  
  return(df)
  
}
