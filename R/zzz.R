#' Squash the global variable notes when building a package. 
#' 
#' @name zzz
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "rowid", "time", "distance", "time_lag", "latitude", "longitude", 
    "ID", "cell", "id_sf", "cell_number", "name", "layer", "variable", "value",
    "raster", "geom", "part", "hole", "x", "y", "distance_sum", "lat", "lon",
    "day", "sunrise", "sunset", "daytime"
  )
  
  # Squash the notes
  utils::globalVariables(variables)
  
}
