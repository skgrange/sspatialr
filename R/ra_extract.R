#' Function to extract values from a \strong{terra} \code{SpatRaster} object for
#' \strong{sf} points.
#' 
#' @author Stuart K. Grange
#' 
#' @param ra A \code{SpatRaster} object.
#' 
#' @param sf A \strong{sf} object. \code{sf} can be points, lines, or polygons.
#' 
#' @param drop_ids Should identifiers from the raster object (\code{cell_number} 
#' and \code{layer}) be dropped from the return? 
#' 
#' @param na.rm Should missing values in the \code{ra} object be removed? 
#' 
#' @param warn Should the function raise warnings? Lower-level GDAL warnings can
#' be raised for myriad reasons, but they are often messages, and are not "true"
#' warnings and therefore can be suppressed. 
#' 
#' @param verbose Should the function give messages?
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{ra_read}}, \code{\link{ra_extract_nested}}
#' 
#' @export
ra_extract <- function(ra, sf, drop_ids = FALSE, warn = TRUE, na.rm = FALSE, 
                      verbose = FALSE) {
  
  # Check inputs
  stopifnot(inherits(ra, "SpatRaster") & inherits(sf, "sf"))
  
  # A message to the user
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Extracting values from raster object...")
  }
  
  # Get dates
  date <- unique(terra::time(ra))
  
  # Create a tibble for joining, when there are additional dimensions the dates
  # are duplicated
  if (!is.na(date[1])) {
    df_dates <- tibble(layer = seq_along(date), date)
  } else {
    df_dates <- tibble()
  }
  
  # Extract values from raster object, one row for each row of sf
  df <- ra_extract_and_clean(ra, sf, df_dates, warn)
  
  # Return empty tibbles here
  if (nrow(df) == 0L) {
    return(df)
  }
  
  # Drop missing values
  if (na.rm) {
    df <- filter(df, !is.na(value))
  }
  
  # Drop raster ids if desired
  if (drop_ids) {
    df <- select(df, -cell_number, -dplyr::matches("layer"))
  }
  
  return(df)
  
}


ra_extract_and_clean <- function(ra, sf, df_dates, warn) {
  
  # Extract the values for each point
  if (warn) {
    df <- terra::extract(
      ra, 
      sf, 
      fun = NULL, 
      method = "simple", 
      ID = TRUE, 
      cells = TRUE
    )
  } else {
    df <- extract_quiet(
      ra, 
      sf, 
      fun = NULL, 
      method = "simple", 
      ID = TRUE, 
      cells = TRUE
    )$result
  }
  
  # Format the return a bit
  df <- df %>% 
    rename(id_sf = ID,
           cell_number = cell) %>% 
    mutate(across(c(id_sf, cell_number), as.integer)) %>% 
    as_tibble()
  
  # What rows/points have no data returned?
  index_all_missing <- apply(
    select(df, -id_sf, -cell_number), 1, function(x) all(is.na(x))
  )
  
  # Return empty tibble if no data are returned
  if (all(index_all_missing)) {
    return(tibble())
  }
  
  # Drop missing points and make longer
  df_long <- df %>% 
    filter(!index_all_missing) %>% 
    tidyr::pivot_longer(-c(id_sf, cell_number), names_to = "name")
  
  # If there are no dates, return here
  if (nrow(df_dates) == 0L) {
    df_long <- rename(df_long, variable = name)
    return(df_long)
  }
  
  # Separate the variables
  n_name_delim <- stringr::str_count(df_long$name[1], "_")
  
  if (n_name_delim == 1L) {
    df_long <- df_long %>% 
      tidyr::separate_wider_delim(
        name, names = c("variable", "layer"), delim = "_"
      )
  } else {
    # If more than one separator
    df_long <- df_long %>% 
      tidyr::separate_wider_delim(
        name, 
        names = c("variable", "dimension", "layer"), 
        delim = "_",
        too_many = "merge"
      )
  }
  
  # Join dates and arrange variables
  df_long <- df_long %>% 
    mutate(layer = as.integer(layer)) %>% 
    left_join(df_dates, by = join_by(layer))  %>% 
    relocate(id_sf,
             cell_number,
             layer, 
             variable,
             dplyr::matches("dimension"), 
             date,
             value)
  
  return(df_long)
  
}


extract_quiet <- purrr::quietly(terra::extract)
