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
#' @param hill_shading Should a transparent hill shading layer be added too? 
#' 
#' @return Leaflet map/plot. 
#' 
#' @export
plot_sf_leaflet <- function(sf, popup = TRUE, transform = TRUE, 
                            hill_shading = FALSE) {
  
  # Keep the handling of raster objects simple for now
  if (inherits(sf, "SpatRaster")) {
    plot <- plot_sf_leaflet_raster_objects(sf)
    return(plot)
  }
  
  # Transform projection system
  if (transform) {
    sf <- sf::st_transform(sf, crs = crs_wgs84)
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
        sf, zcol = names(popup_object), row.numbers = FALSE, feature.id = FALSE
      )
    }
    
  } else {
    popup_object <- NULL
  }
  
  # Set-up the start of the map
  plot <- sf %>% 
    leaflet::leaflet() %>% 
    leaflet::addTiles(options = leaflet::tileOptions(maxZoom = 20))
  
  # Add the specific layers based on the geometry type
  if (any(sf_class(sf) %in% c("point", "multipoint"))) {
    plot <- leaflet::addCircleMarkers(plot, popup = popup_object)
  } else if (any(sf_class(sf) %in% c("linestring", "multilinestring"))) {
    plot <- leaflet::addPolylines(plot, popup = popup_object)
  } else if (any(sf_class(sf) %in% c("polygon", "multipolygon"))) {
    plot <- leaflet::addPolygons(plot, popup = popup_object)
  }
  
  # Add hill shading if desired
  if (hill_shading) {
    plot <- plot %>% 
      leaflet::addProviderTiles(
        "Esri.WorldShadedRelief",
        options = leaflet::providerTileOptions(opacity = 0.3, maxZoom = 13)
      )
  }
  
  return(plot)
  
}


plot_sf_leaflet_raster_objects <- function(ra) {
  terra::plet(ra, tiles = "Streets")
}
