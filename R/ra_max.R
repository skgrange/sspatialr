#' Functions to find which cells in a \strong{terra} object contains the maximum
#' and minimum values. 
#' 
#' @author Stuart K. Grange
#' 
#' @param ra A \strong{terra} \code{SpatRaster} layer.
#' 
#' @return \strong{sf} points.
#' 
#' @export
ra_max <- function(ra) {
  ra_which(ra, statistic = "max")
}


#' @rdname ra_max
#' @export
ra_min <- function(ra) {
  ra_which(ra, statistic = "min")
}


ra_which <- function(ra, statistic = "min") {
  
  # Check data type
  stopifnot(inherits(ra, "SpatRaster"))
  
  # Store projection system of terra object
  crs_ra <- as.integer(terra::crs(ra, describe = TRUE, proj = TRUE)$code)
  
  # Get n value and cell number
  if (statistic == "min") {
    list_where <- terra::where.min(ra)
  } else if (statistic == "max") {
    list_where <- terra::where.max(ra)
  } else {
    cli::cli_abort("`statistic` not recognised.")
  }
  
  # Get coordinates from cell number
  matrix_coordinates <- terra::xyFromCell(ra, list_where[, 2])
  
  # To sf points
  sf <- matrix_coordinates %>% 
    as_tibble() %>% 
    mutate(cell_number = list_where[, 2],
           value = list_where[, 3]) %>% 
    sf_from_df(coords = c("x", "y"), crs = crs_ra)
  
  return(sf)
  
}
