#' Function to calculate daytimes and add a \code{daytime} variable to a tibble.
#' 
#' The sunrise and sunset calculations are done by \strong{suncalc}'s 
#' \code{\link{getSunlightTimes}} function.
#' 
#' @param df Input tibble to be transformed. \code{df} must contain
#' \code{date}, \code{latitude}, and \code{longitude} variables. The 
#' \code{"date"} variable must be of \code{POSIXct} class. 
#' 
#' @param as_factor Should the \code{daytime} variable be a factor? 
#' 
#' @return Tibble, \code{df} with an additional \code{daytime} variable. 
#' 
#' @seealso \code{\link{getSunlightTimes}}
#' 
#' @export
add_daytime <- function(df, as_factor = FALSE) {
  
  # Check if the variables exist
  stopifnot(c("date", "latitude", "longitude") %in% names(df))
  
  # Check for the correct data type
  stopifnot(inherits(df$date, c("POSIXct", "Date")))
  
  # Get time zone of date
  time_zone <- attr(df$date, "tzone")
  
  # Calculate the sunrise and sunset times
  df_days <- df %>% 
    mutate(date = lubridate::floor_date(date, "day")) %>% 
    distinct(date,
             latitude,
             longitude) %>% 
    mutate(date = as.Date(date)) %>% 
    rename(lat = latitude,
           lon = longitude) %>% 
    suncalc::getSunlightTimes(
      data = ., keep = c("sunrise", "sunset"), tz = time_zone
    ) %>% 
    as_tibble() %>% 
    mutate(day = as.POSIXct(date, tz = "UTC")) %>% 
    select(-date) %>% 
    rename(latitude = lat,
           longitude = lon) %>% 
    relocate(day)
  
  # Join sunrise and sunset times and create new variable
  df_join <- df %>% 
    mutate(day = lubridate::floor_date(date, "day")) %>% 
    left_join(
      df_days,
      by = join_by(
        day == day,
        latitude == latitude,
        longitude == longitude
      )
    ) %>% 
    mutate(daytime = if_else(between(date, sunrise, sunset), TRUE, FALSE)) %>% 
    select(-day,
           -sunrise,
           -sunset)
  
  # To a factor
  if (as_factor) {
    df_join <- df_join %>% 
      mutate(daytime = if_else(daytime, "daytime", "nighttime"),
             daytime = factor(daytime, levels = c("daytime", "nighttime")))  
  }
  
  return(df_join)
  
}
