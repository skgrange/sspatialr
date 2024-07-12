#' Function to write a \strong{sf} spatial object to a GPX file. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} spatial object. 
#' 
#' @param file File name to export. 
#' 
#' @param transform Should the projection system be transformed to WGS84 before
#' plotting? 
#' 
#' @return Invisible \code{file}. 
#' 
#' @export
write_gpx <- function(sf, file, transform = FALSE) {
  
  # Expand path
  file <- fs::path_expand(file)
  
  # Transform polygons to lines if needed
  if (sf_class(sf) == "polygon") {
    cli::cli_alert_info(
      "Polygons are not supported by GPX, polygons have been coerced to lines..."
    )
    sf <- sf::st_cast(sf, "LINESTRING", warn = FALSE)
  } else if (sf_class(sf) == "multipolygon") {
    cli::cli_alert_info(
      "Multi polygons are not supported by GPX, polygons have been coerced to lines..."
    )
    sf <- sf %>% 
      sf::st_cast("POLYGON", warn = FALSE) %>% 
      sf::st_cast("LINESTRING", warn = FALSE)
  }
  
  # Export gpx file
  sf::write_sf(
    sf, file, delete_dsn = TRUE, dataset_options = "GPX_USE_EXTENSIONS=true"
  )
  
  return(invisible(file))
  
}
