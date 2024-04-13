#' Function to create a mesh or grid of polygons to cover an sf object, 
#' typically a larger polygon.
#' 
#' @param sf Input \code{sf} object, typically a polygon.
#' 
#' @param cellsize Numeric vector of length \code{1} or \code{2} with the target 
#' cellsize. 
#' 
#' @param n Integer vector of length \code{1} or \code{2}, number of grid cells
#' in the \code{x} or \code{y} directions. 
#' 
#' @param hexagonal Should a hexagonal (not square) mesh be made? 
#' 
#' @param filter Should the mesh be filtered to \code{sf}?
#' 
#' @param intersect Should the cropped/clipped to \code{sf}?
#' 
#' @return A \code{sf} object of polygon geometry type.
#' 
#' @seealso \code{\link{st_make_grid}}
#' 
#' @export
sf_create_mesh <- function(sf, cellsize = NA, n = 10, hexagonal = TRUE,
                           filter = FALSE, intersect = FALSE) {
  
  # Switch arguments, no need to have both here
  if (filter && intersect) {
    filter <- FALSE
  }
  
  # The default for sf
  if (is.na(cellsize[1])) {
    cellsize <- c(diff(st_bbox(sf)[c(1, 3)]), diff(st_bbox(sf)[c(2, 4)])) / n
  }
  
  # Make the grid and promote to sf
  sf_mesh <- sf %>% 
    st_make_grid(
      cellsize = cellsize, n = n, square = !hexagonal, what = "polygons"
    ) %>% 
    st_as_sf()
  
  # Filter grid
  if (filter) {
    sf_mesh <- st_filter(sf_mesh, sf)
  }
  
  # Crop grid, drop points if they exist and only return polygons, warning 
  # suppression is for casting multipolgons and their attribute
  if (intersect) {
    suppressWarnings(
      sf_mesh <- sf_mesh %>% 
        st_intersection(sf) %>% 
        filter(st_geometry_type(.) != "POINT") %>% 
        st_cast("POLYGON", warn = FALSE) 
    )
  }
  
  return(sf_mesh)
  
}
