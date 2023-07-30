#' Function to plot spatial objects with an interactive \strong{leaflet} plot. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} spatial object. 
#' 
#' @param transform Should the projection system be transformed to WGS84 before
#' plotting? 
#' 
#' @return Leaflet map/plot. 
#' 
#' @export
plot_sf_leaflet <- function(sf, transform = TRUE) {
  
  # Transform projection system
  if (transform) {
    sf <- sf::st_transform(sf, crs = crs_wgs_84)
  }
  
  # sf_class(sf)
  
  # Build plot
  plot <- sf %>% 
    leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addPolygons()
  
  return(plot)
  
}
