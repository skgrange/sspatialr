#' Function to transform a \strong{sf} spatial object to a "long" data 
#' frame/tibble.
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} spatial object. 
#' 
#' @return Tibble. 
#' 
#' @export
sf_to_df <- function(sf) {
  
  # Logic depends on geometry type
  if (sf_class(sf) %in% c("point", "multipoint", "linestring")) {
    
    # Keep non-spatial variables
    df_non_spatial <- sf::st_drop_geometry(sf) %>% 
      tibble::rowid_to_column("rowid")
    
    # Extract coordinates
    df_coordinates <- sf %>% 
      sf::st_coordinates() %>% 
      as_tibble()
    
    # Give names to coordinate variables
    if (ncol(df_coordinates) == 2L) {
      df_coordinates <- purrr::set_names(df_coordinates, c("x", "y"))
    } else if (ncol(df_coordinates) == 3L) {
      df_coordinates <- purrr::set_names(df_coordinates, c("x", "y", "rowid"))
    } 
    
    # Bind or join the non-spatial variables with coordinates
    if (ncol(df_coordinates) == 2L) {
      # Bind the two types of variables
      df <- dplyr::bind_cols(df_non_spatial, df_coordinates)  
    } else if (ncol(df_coordinates) == 3L) {
      df <- df_coordinates %>% 
        left_join(df_non_spatial, by = join_by(rowid)) %>% 
        select(-rowid)
    }
    
  } else {
    df <- sf_to_df_beyond_points(sf)
  }
  
  return(df)
  
}


# https://gist.github.com/mdsumner/76c935f7432b4cc1b3ff4d2e50ab4edd
# Edited a little bit
# Worker for sf vector/matrix coords
sf_to_df_beyond_points <- function(x, ...) as_tibble(object_as_df(x))


## XY only
m_or_v_XY <- function(x) {
  x <- unclass(x)
  if (is.null(dim(x))) x <- matrix(x, nrow = 1L)
  x[, 1:2, drop = FALSE]
}


# turn sf coords into a data frame
df_data <- function(x) stats::setNames(as.data.frame(m_or_v_XY(x)), c("x", "y"))


## convert everything to a flat list of data frames
## (hierarchy matches sp, everything is a POLYGON/MULTILINESTRING)
paths_as_df <- function(x) {
  x <- st_cast(x, "MULTILINESTRING")
  rapply(unclass(x), f = df_data, classes = c("numeric", "matrix"), how = "list")
}


object_as_df <- function(x) {
  dat <- dplyr::bind_rows(
    lapply(
      st_geometry(x), function(a) dplyr::bind_rows(paths_as_df(a), .id = "group")),
    .id = "id"
  )
  dat$piece <- factor(dat$group)
  dat$group <- factor(paste0(dat$id, dat$group))
  dat
}
