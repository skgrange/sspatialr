#' Function to transform a \strong{terra} layer to contour lines.
#' 
#' @author Stuart K. Grange
#' 
#' @param ra A \strong{terra} \code{SpatRaster} layer.
#' 
#' @param levels Number of levels to split the numeric scale of \code{ra} into. 
#' 
#' @return \strong{sf} lines.
#' 
#' @export
ra_to_contours <- function(ra, levels = 20) {
  
  # Check data type
  stopifnot(inherits(ra, "SpatRaster"))
  
  # Get name of layers
  variable_names <- names(ra)
  
  # To contours and on to sf lines
  sf <- ra %>% 
    terra::as.contour(nlevels = levels) %>% 
    stats::setNames(variable_names[1]) %>% 
    sf::st_as_sf() %>% 
    sf::st_cast("MULTILINESTRING")
  
  return(sf)
  
}
