#' Function to calculate sunlight times. 
#' 
#' The calculations are done the \code{\link[suncalc]{getSunlightTimes}} 
#' function in the \strong{suncalc} package.
#' 
#' @param df Input data frame or tibble with \code{date}, \code{latitude}, and 
#' \code{longitude} variables.
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link[suncalc]{getSunlightTimes}}
#' 
#' @examples
#' 
#' # Create a mapping table with the correctly named inputs for the Vatican City
#' data_mapping <- data.frame(
#'   date = Sys.Date(), latitude = 41.90223, longitude = 12.45637
#' )
#' 
#' # Calculate sunlight time
#' calculate_sunlight_times(data_mapping)
#' 
#' @export
calculate_sunlight_times <- function(df) {
  
  # Check if the variables exist
  stopifnot(c("date", "latitude", "longitude") %in% names(df))
  
  # Check for the correct data type
  stopifnot(inherits(df$date, "Date"))
  
  # Calculate the different sunlight times
  df_times <- df %>%
    distinct(date,
             latitude,
             longitude) %>%
    rename(lat = latitude,
           lon = longitude) %>%
    suncalc::getSunlightTimes(data = ., tz = "UTC") %>%
    as_tibble() %>%
    dplyr::rename_with(threadr::str_to_underscore) %>%
    rename(latitude = lat,
           longitude = lon)
  
  # Join input tibble with sunlight times
  df_join <- left_join(df, df_times, by = join_by(latitude, longitude, date))
  
  return(df_join)
  
}
