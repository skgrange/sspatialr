#' Function to return the start and end points of a \strong{sf} line object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} line object. 
#' 
#' @return \strong{sf} point object. 
#' 
#' @export
sf_start_point <- function(sf) {
  
  sf %>% 
    lwgeom::st_startpoint() %>% 
    sf::st_as_sf() %>% 
    rename(geometry = x)
  
} 


#' @rdname sf_start_point
#' @export
sf_end_point <- function(sf) {
  
  sf %>% 
    lwgeom::st_endpoint() %>% 
    sf::st_as_sf() %>% 
    rename(geometry = x)
  
}
