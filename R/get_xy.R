# ------------------------------------------------------------------------------
# .get_xy()
#
# Date: 2019-08-20
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
.get_xy <- function(address, sp, varname, verbose = FALSE) {
  
  if (inherits(sp, "SpatialPolygons")) {
    if (verbose) 
      warning("SpatialPolygons or one that inherits from it is provided.\n",
              "The output coordinates are approximate only.", call. = FALSE)
    xy <- coordinates(sp)
  } 
  
  else if (inherits(sp, "SpatialPoints")) {
    xy <- coordinates(sp)
  } 
  
  else {
    stop("'sp' must be one of the following polygons:\n",
         "  SpatialPoints, SpatialPointsDataFrame,\n",
         "  SpatialPolygons, SpatialPolygonsDataFrame")
  }
  
  matching_table <- data.frame(sp)[, names(sp) == varname]
  ID <- match(address, matching_table)
  
  return(xy[ID,])
}

