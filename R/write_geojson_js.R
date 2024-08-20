#' Function to export an \code{sf} object as GeoJSON with a JavaScript variable 
#' name, usually for mapping with Leaflet. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} object. 
#' 
#' @param file File to export the \strong{sf} object to. 
#' 
#' @param name The JavaScript variable name to give the GeoJSON object. 
#' 
#' @param pretty Should the exported GeoJSON be pretty printed? 
#' 
#' @param round Number of digits to round coordinates to. 
#' 
#' @return An invisible GeoJSON with JavaScript name character vector. 
#' 
#' @export 
write_geojson_js <- function(sf, file, name = NA, pretty = TRUE, round = NA) {
  
  # Give name if one has not been passed
  if (is.na(name[1])) {
    name <- "spatial_object"
  }
  
  # Create
  json <- create_geojson(sf, pretty, round)
  
  # Add the js formatting for an object
  json_js <- stringr::str_c("var ", name, " = [", json, "];")
  
  # Write string to disk
  write(json_js, file)
  
  return(invisible(json_js))
  
}


# No export
create_geojson <- function(sf, pretty, round) {
  
  # Make json string, will also work for data frames sometimes but will give
  # message
  json <- suppressMessages(
    geojsonio::geojson_json(sf)
  )
  
  # Use jsonlite to do a better job of pretty printing, expensive though
  if (pretty || !is.na(round)) {
    
    # Parse again
    json <- jsonlite::fromJSON(json)
    
    # Max precision is needed here
    json <- jsonlite::toJSON(
      json, 
      pretty = TRUE, 
      auto_unbox = TRUE, 
      digits = round
    )
    
  }
  
  return(json)
  
}
