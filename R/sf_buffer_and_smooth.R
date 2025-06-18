#' Function to buffer a \strong{sf} object and subsequently smooth the buffered
#' output. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} object. 
#' 
#' @param buffer Buffer distance. See \code{\link[sf:geos_unary]{st_buffer}} for 
#' details. 
#' 
#' @param smoothness The smoothness and level of generalisation parameter.
#' 
#' @param tolerance The tolerance parameter.

#' @return \strong{sf} buffered and smoothed. 
#' 
#' @export
sf_buffer_and_smooth <- function(sf, buffer = 1000, smoothness = 15, 
                                 tolerance = 0.1) {
  
  sf %>% 
    sf::st_buffer(dist = buffer) %>% 
    smoothr::smooth(method = "ksmooth", smoothness = smoothness) %>% 
    sf::st_simplify(preserveTopology = TRUE, dTolerance = tolerance) 
  
}
