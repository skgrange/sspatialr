#' Function to get extreme points for a \code{sf} object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \code{sf} object. 
#' 
#' @return \code{sf} points.
#' 
#' @export
sf_extreme_points <- function(sf) {
  
  # Get a coordinates matrix
  coordinates <- sf::st_coordinates(sf)
  
  # Get extreme row indices
  id_extreme <- c(
    north = which.max(coordinates[, 2]),
    south = which.min(coordinates[, 2]),
    east = which.max(coordinates[, 1]),
    west = which.min(coordinates[, 1])
  )
  
  # Make a sf points object
  sf_points <- tibble::tibble(
    dimension = names(id_extreme),
    longitude = coordinates[id_extreme, 1],
    latitude  = coordinates[id_extreme, 2]
  ) |> 
    sf_from_df(crs = sf::st_crs(sf))
  
  return(sf_points)
  
}
