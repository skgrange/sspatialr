#' Function to transform a raster object to a tibble. 
#' 
#' @param ra A \strong{terra} \code{SpatRaster} object.
#' 
#' @param variable_as_date If \code{ra}'s names represent dates, should the 
#' dates be parsed and the variable renamed? 
#' 
#' @param tz If \code{variable_as_date} is \code{TRUE}, what time zone are the 
#' dates stored in? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
ra_to_df <- function(ra, variable_as_date = FALSE, tz = "UTC") {
  
  # Check input
  stopifnot(inherits(ra, "SpatRaster"))
  
  # To terra's version of spatial points (a spat vector)
  ra <- terra::as.points(ra)
  
  # To sf
  # sf::st_as_sf(ra)
  
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
  
  # Make data longer
  df <- tidyr::pivot_longer(df, -c(geom, part, hole, x, y), names_to = "variable")
  
  # Parse dates and rename variable
  if (variable_as_date && stringr::str_detect(df$variable[1], "^X")) {
    df <- df %>% 
      rename(date = variable) %>% 
      mutate(date = stringr::str_remove(date, "^X"), 
             date = lubridate::ymd_hms(date, tz = tz, truncated = 3))
  }
  
  return(df)
  
}
