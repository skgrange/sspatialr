#' Function to read a raster data file as a \strong{terra} \code{SpatRaster} 
#' object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Raster file name to read. 
#' 
#' @param variable A vector of the variables within \code{file} to be read. 
#' 
#' @param layers An integer or character to select from \code{file}. 
#' 
#' @return \code{SpatRaster} object.
#' 
#' @export
ra_read <- function(file, variable = NA, layers = NULL) {
  
  # Switch varaible input for all variables
  if (is.na(variable[1])) variable <- 0L
  
  # Read file
  ra <- terra::rast(file, subds = variable, lyrs = layers)
  
  return(ra)
  
}


#' Function to read a raster file in a nested tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Raster file name to read. 
#' 
#' @return Nested tibble grouped by rows. 
#' 
#' @seealso [ra_read()]
#' 
#' @export
ra_read_nested <- function(file) {
  
  # Load entire file
  ra <- terra::rast(file)
  
  # Get variables
  variables <- terra::varnames(ra)
  
  # Put each variable into separate elements
  list_raster <- purrr::map(variables, ~ra[.])
  
  # Make a rowwise nested tibble
  df_nest <- tibble(
    file = file,
    variable = variables,
    raster = list_raster
  ) %>% 
    rowwise(file,
            variable)
  
  return(df_nest)
  
}
