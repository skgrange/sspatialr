#' Function to create WKT geometry strings with string processing functions to 
#' allow for potentially missing geometries.
#' 
#' @author Stuart K. Grange
#' 
#' @param x \code{x}/longitude vector. 
#' 
#' @param y \code{y}/latitude numeric vector. 
#' 
#' @param geometry_type Type of geometry to create. Only \code{"points"} are 
#' supported. 
#' 
#' @param round Decimal points to round \code{x} and \code{y} vectors. 
#' 
#' @return Character vector.
#' 
#' @export
coords_to_wkt <- function(x, y, geometry_type = "points", round = NA) {
  
  # Check class of inputs
  stopifnot(inherits(x, "numeric"), inherits(y, "numeric"))
  
  # Round coordinates
  if (!is.na(round[1])) {
    x <- round(x, digits = round)
    y <- round(y, digits = round)
  }
  
  # Build wkt
  if (geometry_type == "points") {
    geometry <- stringr::str_c("POINT (", x, " ", y, ")")
  } else {
    cli::cli_abort("`geometry_type` not suported.")
  }
  
  return(geometry)
  
}
