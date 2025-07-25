#' Built-in projection/coordinate reference system constants. 
#' 
#' @author Stuart K. Grange
#' 
#' @param crs Coordinate reference system integer. 
#' 
#' @seealso \code{\link[sf]{st_transform}}, \code{\link[terra]{project}}
#' 
#' @return Integer with length of \code{1}.
#' 
#' @export
crs_wgs84 <- 4326L


#' @rdname crs_wgs84
#' @export
crs_swiss <- 21781L


#' @rdname crs_wgs84
#' @export
crs_swiss_plus <- 2056L


#' @rdname crs_wgs84
#' @export
crs_bng <- 27700L


#' @rdname crs_wgs84
#' @export
crs_nztm <- 2193L


#' @rdname crs_wgs84
#' @export
crs_gda_94 <- 28356L


#' @rdname crs_wgs84
#' @export
crs_gda_2020 <- 7844L


#' @rdname crs_wgs84
#' @export
crs_with_prefix <- function(crs) {
  stringr::str_c("epsg:", crs)
}
