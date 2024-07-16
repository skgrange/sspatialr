#' Function to create a \code{sf} bounding box for a \code{sf} object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \code{sf} object.
#' 
#' @param buffer An optional buffer to apply before calculating the bounding
#' box. 
#' 
#' @return A \code{sf} polygon. 
#' 
#' @export
sf_create_bounding_box <- function(sf, buffer = NA) {
  
  # Buffer object
  if (!is.na(buffer)) {
    sf <- sf::st_buffer(sf, dist = buffer)
  }
  
  # Get bounding box and promote to a sf polygon
  sf <- sf %>% 
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_sf()
  
  return(sf)
  
}
