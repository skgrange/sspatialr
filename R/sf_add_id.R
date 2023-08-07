#' Function to add an integer ID to a \code{sf} object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \code{sf} object.
#' 
#' @param name The name of the ID variable. 
#' 
#' @return \code{sf} with an additional ID variable. 
#' 
#' @export
sf_add_id <- function(sf, name = "id_sf") {
  
  # Check input
  stopifnot(inherits(sf, "sf"))
  
  # Add an integer id as the first column
  sf <- tibble::rowid_to_column(sf, var = name)
  
  return(sf)
  
}
