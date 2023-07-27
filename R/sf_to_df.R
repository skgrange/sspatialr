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
  df_non_spatial <- sf::st_drop_geometry(sf)
  
  # Extract coordinates
  df_coordinates <- sf %>% 
    sf::st_coordinates() %>% 
    as_tibble() %>% 
    purrr::set_names(c("x", "y"))
  
  # Bind the two types of variables
  df <- dplyr::bind_cols(df_non_spatial, df_coordinates)
  
  return(df)
  
}
