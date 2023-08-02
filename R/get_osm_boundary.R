#' Function to get OpenStreetMap boundaries as \strong{sf} polygons.
#' 
#' \code{get_osm_boundary} accesses 
#' \href{http://polygons.openstreetmap.fr/index.py}{this} polygon creator tool. 
#' 
#' @param id A vector of OpenStreetMap relations. An integer key.
#' 
#' @param sleep Number of seconds between server queries. This is useful if 
#' many large polygons are being requested. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return \strong{sf} polygons. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Get North York Moors National Park boundary
#' sf_moors <- get_osm_boundary(409150)
#' 
#' @export
get_osm_boundary <- function(id, sleep = 1, verbose = FALSE, progress = FALSE) {
  
  # Do, use bind_rows for sf objects, passing id rather than url because it is
  # used within the worker
  id %>% 
    purrr::map(
      ~get_osm_boundary_worker(
        ., sleep = sleep, verbose = verbose
      ),
      .progress = progress
    ) %>% 
    bind_rows()
  
}


get_osm_boundary_worker <- function(id, sleep, verbose) {
  
  # Build url
  url <- glue::glue(
    "http://polygons.openstreetmap.fr/get_wkt.py?id={id}&params=0"
  )
  
  # Message to user
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} `{url}`...")
  }
  
  # Get wkt
  text <- tryCatch({
    threadr::read_lines(url)
  }, error = function(e) {
    cli::cli_alert_info("{threadr::cli_date()} `{url}` returned no data...")
    NULL
  })
  
  # Parse wkt and add id
  sf <- text %>% 
    sf::st_as_sfc() %>% 
    sf::st_sf() %>% 
    mutate(id = !!as.integer(id)) %>% 
    relocate(id)
  
  # Sleep if needed
  Sys.sleep(sleep)
  
  return(sf)
  
}
