#' Function to plot spatial objects with an interactive \strong{leaflet} plot. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} spatial object. 
#' 
#' @param popup Should interactive popups be included in the map if possible? 
#' 
#' @param transform Should the projection system be transformed to WGS84 before
#' plotting? 
#' 
#' @return Leaflet map/plot. 
#' 
#' @export
plot_sf_leaflet <- function(sf, popup = TRUE, transform = TRUE) {
  
  # Transform projection system
  if (transform) {
    sf <- sf::st_transform(sf, crs = crs_wgs_84)
  }
  
  # Build a popup object 
  if (popup) {
    
    # Drop geometry variable
    popup_object <- sf::st_drop_geometry(sf)
    
    # Are there other variables? 
    if (ncol(popup_object) == 0L) {
      popup_object <- NULL
    } else {
      popup_object <- leafpop::popupTable(
        popup_object, row.numbers = FALSE, feature.id = FALSE
      )
    }
    
  } else {
    popup_object <- NULL
  }
  
  # Set-up the start of the map
  plot <- sf %>% 
    leaflet::leaflet() %>% 
    leaflet::addTiles()
  
  # Add the specific layers
  if (sf_class(sf) == "point") {
    plot <- leaflet::addCircleMarkers(plot, popup = popup_object)
  } else if (sf_class(sf) == "linestring") {
    plot <- leaflet::addPolylines(plot, popup = popup_object)
  } else if (sf_class(sf) == "polygon") {
    plot <- leaflet::addPolygons(plot, popup = popup_object)
  }
  
  return(plot)
  
}