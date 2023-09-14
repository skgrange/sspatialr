#' Function to interpolate a raster object with an inverse distance weighted
#' method. 
#' 
#' @author Stuart K. Grange
#' 
#' @param ra A \code{SpatRaster} object.
#' 
#' @param variable Variable name to interpolate in \code{ra}. 
#' 
#' @param sf A \strong{sf} object, usually a \strong{sf} points object/ 
#' 
#' @param n_max The number of nearest observations that should be used for 
#' interpolation. 
#' 
#' @param na.rm Should \code{NA} be omitted? 
#' 
#' @return A \code{SpatRaster} object.
#' 
#' @export
ra_interpolate <- function(ra, sf, variable = "value", n_max = Inf, 
                           na.rm = FALSE) {
  
  # Check spatial inputs
  stopifnot(inherits(ra, "SpatRaster"), inherits(sf, "sf"))
  
  # Build model formula
  formula <- stringr::str_c(variable, " ~ 1")
  formula <- stats::as.formula(formula)
  
  # Model with gstat
  list_model <- gstat::gstat(formula = formula, data = sf, nmax = n_max)
  
  # Do the spatial interpolation without printing messages
  ra <- threadr::quiet(
    terra::interpolate(
      ra, list_model, fun = ra_interpolate_predict_worker, na.rm = na.rm
    )
  )
  
  return(ra)
  
}


ra_interpolate_predict_worker <- function(model, x, ...) {
  
  # Do the prediction
  v <- sf::st_as_sf(
    x, coords = c("x", "y"), crs = terra::crs(model$data[[1]]$data)
  )
  
  sf_predict <- stats::predict(model, v, ...)
  
  # Extract the prediction as a numeric vector
  x <- sf_predict %>% 
    sf::st_drop_geometry() %>% 
    pull(1)
  
  return(x)
  
}
