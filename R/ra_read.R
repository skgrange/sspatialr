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
#' @seealso \code{\link{ra_read_nested}}
#' 
#' @export
ra_read <- function(file, variable = NA, layers = NULL) {
  
  # Switch variable input for all variables
  if (is.na(variable[1])) variable <- 0L
  
  # Read file
  ra <- terra::rast(file, subds = variable, lyrs = layers)
  
  return(ra)
  
}


#' Function to read a raster file in a nested tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file A vector of raster files name to read. 
#' 
#' @param separate_layers Should all the layers within a variable 
#' (\strong{terra}'s \code{varname}) be read separately? Not usually desired for
#' time series, but for some data sources the layers within a variable form 
#' distinct variables. 
#' 
#' @param warn Should the function raise warnings? Lower-level GDAL warnings can
#' be raised for myriad reasons, but they are often messages, and are not "true"
#' warnings and therefore can be suppressed. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return Nested tibble grouped by rows. 
#' 
#' @seealso \code{\link{ra_read}}
#' 
#' @export
ra_read_nested <- function(file, separate_layers = FALSE, warn = TRUE, 
                           verbose = FALSE, progress = FALSE) {
  
  file %>% 
    purrr::map(
      ra_read_nested_worker, 
      separate_layers = separate_layers,
      warn = warn,
      verbose = verbose, 
      .progress = progress
    ) %>% 
    purrr::list_rbind() %>% 
    rowwise(file,
            variable)
  
}


ra_read_nested_worker <- function(file, separate_layers, warn, verbose) {
  
  # A message to the user
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Reading `{file}`...")
  }
  
  # Load entire file, warnings can be raised for all sorts of low-level reasons
  # but can be not raised if desired
  if (warn) {
    ra <- terra::rast(file)
  } else {
    ra <- rast_quiet(file)$result
  }
  
  # Get variable names
  if (!separate_layers) {
    # Just the variables
    variables <- terra::varnames(ra)
  } else {
    # If the layers are to be read individually too, not usually wanted for 
    # time series data but different layers can exist in any given `varname` that
    # form separate variables
    variables <- names(ra)
  }
  
  if (length(variables) == 1L) {
    # If one variable/layer, just put it within a list
    list_raster <- list(ra)
  } else {
    # Put each variable into separate elements
    list_raster <- purrr::map(variables, ~ra[.])
  }
  
  # Make a rowwise nested tibble
  df_nest <- tibble(
    file = file,
    variable = variables,
    raster = list_raster
  )
  
  return(df_nest)
  
}


rast_quiet <- purrr::quietly(terra::rast)
