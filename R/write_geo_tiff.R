#' Functions to export raster objects. 
#' 
#' @param ra Raster object to export.
#' 
#' @param file File to export raster object too. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, \code{file}. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Export raster object as geo tiff 
#' write_geo_tiff(ra, "raster_object.tif")
#' 
#' # Or text file
#' write_esri_grid_ascii(ra, "raster_object.grd")
#' 
#' }
#' 
#' @export
write_geo_tiff <- function(ra, file) {
  
  # Expand path
  file <- fs::path_expand(file)
  
  # Check file extension
  stopifnot(
    stringr::str_to_lower(fs::path_ext(file)) %in%
      c("tif", "tiff", "geotif", "geotiff")
  )
  
  # Write file
  terra::writeRaster(ra, file, overwrite = TRUE)
  
  return(invisible(file))
  
}


#' @rdname write_geo_tiff
#' @export
write_esri_grid_ascii <- function(ra, file) {
  
  # Expand path
  file <- fs::path_expand(file)
  
  # Check file extension
  stopifnot(stringr::str_to_lower(fs::path_ext(file)) %in% c("asc", "grd"))
  
  # Write file
  terra::writeRaster(ra, file, overwrite = TRUE)
  
  return(invisible(file))
  
}
