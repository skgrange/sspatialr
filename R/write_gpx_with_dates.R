#' Function to export GPX files with a \code{time} (date) variable. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame or tibble with at least these three variables: 
#' \code{latitude}, \code{longitude}, and \code{date}. \code{date} also needs 
#' to be of \code{POSIXct} type. If variables with the names \code{elevation} or 
#' \code{temperature} are included in \code{df}, these too will be added to the
#' GPX file. 
#' 
#' @param file File name of GPX export. 
#' 
#' @return Invisible, a GPX mark-up file. 
#' 
#' @export
write_gpx_with_dates <- function(df, file) {
  
  # Check input
  stopifnot(c("latitude", "longitude", "date") %in% names(df))
  stopifnot(lubridate::is.POSIXt(df$date))
  
  # Control what the tibble contains
  df <- df %>% 
    select(date,
           latitude,
           longitude, 
           dplyr::matches("elevation"),
           dplyr::matches("temperature"))
  
  # Drop missing coordinates if they exist and raise a warning
  if (anyNA(c(df$latitude, df$longitude))) {
    df <- filter(df, !is.na(latitude), !is.na(longitude))
    cli::cli_alert_info(
      "Missing coordinates were detected and have been removed..."
    )
  }
  
  # Format dates for gpx files
  df <- df %>% 
    mutate(date = lubridate::with_tz(date, "UTC"),
           date = format(date, format = "%Y-%m-%dT%H:%M:%OS3Z"))
  
  # Detect optional variables
  has_elevation <- "elevation" %in% names(df)
  has_temperature <- "temperature" %in% names(df)
  
  # Check for missing values
  if (has_elevation && anyNA(df$elevation)) {
    cli::cli_abort("GPX files cannot contain missing `elevation` values.")
  }
  
  if (has_temperature && anyNA(df$temperature)) {
    cli::cli_abort("GPX files cannot contain missing `temperature` values.")
  }
  
  # Build the body of the gpx file
  gpx_body <- df %>% 
    purrr::pmap(
      build_gpx_node, 
      has_elevation = has_elevation, 
      has_temperature = has_temperature
    ) %>% 
    purrr::flatten_chr() %>% 
    stringr::str_c(collapse = "\n")
  
  # Combine the different pieces
  gpx_complete <- stringr::str_c(gpx_preamble_tags, gpx_body, gpx_trailing_tags)
  
  # Export to disc, not using readr to avoid a progress bar which is not 
  # required
  writeLines(gpx_complete, file)
  
  return(invisible(gpx_complete))
  
}


build_gpx_node <- function(latitude, longitude, date, elevation, 
                           temperature, has_elevation, has_temperature) {
  
  # TODO: add heart rate too
  
  # Inspired by: 
  # https://stackoverflow.com/questions/51067311/r-convert-gps-to-gpx-with-timestamp
  
  # Build the primary node with or without an elevation variable
  if (has_elevation) {
    x <- stringr::str_glue(
      '<trkpt lat="{latitude}" lon="{longitude}">
         <ele>{elevation}</ele>
         <time>{date}</time>'
    )
  } else {
    x <- stringr::str_glue(
      '<trkpt lat="{latitude}" lon="{longitude}">
         <time>{date}</time>'
    )
  }
  
  # Add temperature as an ns3 tag too
  if (has_temperature) {
    x <- stringr::str_glue(
      "{x}
         <extensions>
           <ns3:TrackPointExtension>
             <ns3:atemp>{temperature}</ns3:atemp>
           </ns3:TrackPointExtension>
         </extensions>"
    )
  }
  
  # Close the node
  x <- stringr::str_glue(
    "{x}
    </trkpt>"
  )
  
  return(x)
  
}


gpx_preamble_tags <- '<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<gpx version="1.1" creator="sspatialr http://www.gpsvisualizer.com/" 
xmlns="http://www.topografix.com/GPX/1/1" 
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
<trk>
<name>sspatialr export</name>
<trkseg>'


gpx_trailing_tags <- '</trkseg>\n</trk>\n</gpx>'
