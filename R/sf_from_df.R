#' Function to promote a data frame or tibble to a \strong{sf} spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame or tibble containing spatial data to be promoted to a 
#' \strong{sf} spatial object. 
#' 
#' @param geometry_type Type of geometry to promote data to. One of \code{"points"},
#' \code{"lines"}, or \code{"polygons"}.
#' 
#' @param coords What variables in \code{df} contain the \code{x} and \code{y}
#' coordinates (in that order). 
#' 
#' @param by What variable(s) should be used as a grouping variable? 
#' 
#' @param crs What coordinate reference system should are \code{coords} stored
#' in? 
#' 
#' @param geometry_name What variable name should the geometry variable take? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return A \strong{sf} spatial object. 
#' 
#' @export
sf_from_df <- function(df, geometry_type = "points", 
                       coords = c("longitude", "latitude"), by = NULL, 
                       crs = sf::st_crs("wgs84"), geometry_name = "geometry",
                       verbose = TRUE) {
  
  # Check input
  stopifnot(inherits(df, "data.frame"))
  stopifnot(coords %in% names(df))
  
  # Parse type input, used for logic
  geometry_type <- geometry_type %>% 
    stringr::str_to_lower() %>% 
    stringr::str_squish()
  
  # Are there missing coordinates? 
  missing_coordinates <- df %>% 
    select(!!coords) %>% 
    anyNA()
  
  # If so, remove the missing observations
  if (missing_coordinates) {
    if (verbose) {
      cli::cli_alert_info(
        "There are missing coordinates, these have been removed..."
      )
    }
    df <- filter(df, !dplyr::if_any(dplyr::all_of(coords), is.na))
  }
  
  # Promote to points first
  sf <- st_as_sf(
    df, coords = coords, crs = crs, sf_column_name = geometry_name
  )
  
  if (geometry_type == "lines") {
    # Promote to lines
    sf <- sf %>% 
      group_by(across(dplyr::all_of(by))) %>% 
      summarise(do_union = FALSE,
                .groups = "drop") %>% 
      st_cast("LINESTRING")
  } else if (geometry_type == "polygons") {
    # Promote to polygons
    sf <- sf %>% 
      group_by(across(dplyr::all_of(by))) %>% 
      summarise(do_union = FALSE,
                .groups = "drop") %>% 
      st_cast("POLYGON")
  }
  
  return(sf)
  
}
