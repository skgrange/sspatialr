#' Function to extract values from a \strong{terra} \code{SpatRaster} object for
#' \strong{sf} points.
#' 
#' @author Stuart K. Grange
#' 
#' @param ra A \code{SpatRaster} object.
#' 
#' @param sf_points A \strong{sf} points object. 
#' 
#' @param na.rm Should missing values in the \code{ra} object be removed? 
#' 
#' @param verbose Should the function give messages?
#' 
#' @export
ra_extact <- function(ra, sf_points, na.rm = FALSE, verbose = FALSE) {
  
  # Check inputs
  stopifnot(
    inherits(ra, "SpatRaster") & 
      inherits(sf_points, "sf") &
      sf_class(sf_points) == "point"
  )
  
  # Get dates and create a tibble for joining, when there are additional 
  # dimensions the dates are duplicated
  date <- unique(terra::time(ra))
  df_dates <- tibble(layer = seq_along(date), date)
  
  # Extract values from raster object, one row for each row of sf_points
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Extracting values from raster object...")
  }
  
  df <- ra_extract_and_clean(ra, sf_points, df_dates, verbose)
  
  # Drop missing values
  if (na.rm) {
    df <- filter(df, !is.na(value))
  }
  
  return(df)
  
}


ra_extract_and_clean <- function(ra, sf_points, df_dates, verbose) {
  
  # Keep non-spatial variables
  df_points <- sf_points %>% 
    sf::st_drop_geometry() %>% 
    tibble::rowid_to_column("id")
  
  # Extract the values for each point
  df <- terra::extract(
    ra, 
    sf_points, 
    fun = NULL, 
    method = "simple", 
    ID = TRUE, 
    cells = TRUE
  ) %>% 
    rename(id = ID,
           cell_number = cell) %>% 
    mutate(across(c(id, cell_number), as.integer)) %>% 
    as_tibble()
  
  # What rows/points have no data returned?
  index_all_missing <- apply(
    select(df, -id, -cell_number), 1, function(x) all(is.na(x))
  )
  
  # Return empty tibble if no data are returned
  if (all(index_all_missing)) {
    return(tibble())
  }
  
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Formatting extracted data...")
  }
  
  # Drop missing points and make longer
  df_long <- df %>% 
    filter(!index_all_missing) %>% 
    tidyr::pivot_longer(-c(id, cell_number), names_to = "name")
  
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
  
  # Final cleaning
  df_long <- df_long %>% 
    mutate(layer = as.integer(layer)) %>% 
    left_join(df_dates, by = join_by(layer))  %>% 
    left_join(df_points, by = join_by(id)) %>% 
    relocate(names(df_points),
             cell_number,
             layer, 
             variable,
             dplyr::matches("dimension"), 
             date,
             value)
  
  return(df_long)
  
}
