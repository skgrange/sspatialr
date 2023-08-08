#' Function to transform a raster object to a tibble. 
#' 
#' @param ra A \strong{terra} \code{SpatRaster} object.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
ra_to_df <- function(ra) {
  
  # Check input
  stopifnot(inherits(ra, "SpatRaster"))
  
  # To terra's version of spatial points (a spat vector)
  ra <- terra::as.points(ra)
  
  # To sf
  # st_as_sf(ra)
  
  # Bind geom and values together in a tibble and do some minor cleaning
  df <- dplyr::bind_cols(
    terra::geom(ra),
    terra::values(ra)
  ) %>% 
    as_tibble() %>% 
    mutate(across(c(geom, part), as.integer),
           hole = as.logical(hole)) %>% 
    relocate(hole,
             .before = x)
  
  return(df)
  
}
