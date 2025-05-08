#' Function to project a \strong{terra} raster object's CRS projection system. 
#' 
#' @param ra A \strong{terra} raster object.
#' 
#' @param crs A projection/coordinate reference system (CRS) integer. 
#' 
#' @return A \strong{terra} raster object. 
#' 
#' @seealso \code{\link[terra]{project}}, \code{\link[sf]{st_transform}}, 
#' \code{\link{crs_wgs84}}
#' 
#' @export
ra_transform <- function(ra, crs) {
  
  # Check raster object
  stopifnot(inherits(ra, c("SpatRaster", "SpatVector")))
  
  # Add or transform projection system
  if (terra::crs(ra) == "") {
    # Force projection
    cli::cli_alert_info(
      "Raster object has no projection, the projection has been forced..."
    )
    terra::crs(ra) <- crs_with_prefix(crs)
  } else {
    ra <- terra::project(ra, crs_with_prefix(crs))
  }
  
  return(ra)
  
}
