#' Function to extract values from raster (\code{SpatRaster}) objects when 
#' contained in a nested tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df_nest A nested tibble from \code{\link{ra_read_nested}}. 
#' 
#' @param sf A \strong{sf} object. 
#' 
#' @param drop_ids Should identifiers from the raster object (\code{cell_number} 
#' and \code{layer}) be dropped from the return? 
#' 
#' @param na.rm Should missing values in the \code{ra} object be removed? 
#' 
#' @param verbose Should the function give messages?
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return A nested tibble, \code{df_nest} with an additional \code{extract} 
#' variable. 
#' 
#' @seealso \code{\link{ra_read_nested}}, \code{\link{ra_extract}}
#' 
#' @export
ra_extract_nested <- function(df_nest, sf, drop_ids = TRUE, na.rm = FALSE, 
                              verbose = FALSE, progress = FALSE) {
  
  # Check inputs
  stopifnot(c("raster") %in% names(df_nest), inherits(df_nest, "rowwise_df"))
  
  # Pull the raster object and extract the data from the layers with purrr for
  # the progress bar
  list_extract <- df_nest %>% 
    pull(raster) %>% 
    purrr::map(
      ~ra_extract(
        .,
        sf = sf,
        drop_ids = drop_ids, 
        na.rm = na.rm, 
        verbose = verbose
      ),
      .progress = progress
    )
  
  # Add extracted list to nested tibble, use this assignment to keep grouping
  df_nest$extract <- list_extract
  
  return(df_nest)
  
}
