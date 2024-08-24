#' Function to calculate spatial distance between an \code{sf} object with a 
#' single geometry and a second \code{sf} object with many geometries. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \code{sf} object with many geometries. 
#' 
#' @param sf_single A \code{sf} object with a single geometry. 
#' 
#' @param as_integer Should the return be an integer vector? 
#' 
#' @return Numeric or integer vector with the length of \code{sf}. 
#' 
#' @export
st_distance_single <- function(sf, sf_single, as_integer = FALSE) {
  
  # Check second sf object has a single geometry
  stopifnot(nrow(sf_single) == 1L)
  
  # Calculate distance
  distance <- sf::st_distance(sf, sf_single)
  
  # Push to numeric vector and drop unit attribute
  distance <- as.numeric(distance[, 1])
  
  # To an integer
  if (as_integer) {
    distance <- distance %>% 
      round() %>% 
      as.integer()
  }
  
  return(distance)
  
}


#' Function to calculate lengths of a \code{sf} object and return a vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf \code{sf} object. 
#' 
#' @param as_integer Should the return be an integer vector? 
#' 
#' @return Numeric or integer vector. 
#' 
#' @export
st_length_simple <- function(sf, as_integer = FALSE) {
  
  # Calculate length and drop units
  length <- sf %>% 
    sf::st_length() %>% 
    as.numeric()
  
  if (as_integer) {
    length <- length %>% 
      round() %>% 
      as.integer()
  }
  
  return(length)
  
}
