#' Function to write a \strong{sf} spatial object to shapefiles.
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} spatial object. 
#' 
#' @param file File name to export. 
#' 
#' @return Invisible \code{file}. 
#' 
#' @export
write_shapefile <- function(sf, file) {
  
  # Add file extension if needed
  if (stringr::str_to_lower(fs::path_ext(file)) != "shp") {
    file <- file %>% 
      fs::path_ext_remove() %>% 
      fs::path_ext_set("shp")
  } else {
    file <- fs::path_expand(file)
  }
  
  # Write
  sf::write_sf(sf, file, delete_layer = TRUE)
  
  return(invisible(file))
  
}
