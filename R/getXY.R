# ------------------------------------------------------------------------------
# Utility functgions for class 'ASpaces'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# ------------------------------------------------------------------------------

getXY <- function(address, sp, varname) {
  
  if (inherits(sp, "SpatialPolygons") | inherits(sp, "SpatialPoints")) {
    xy <- coordinates(sp)
  } else {
    stop("'sp' must be one of the following polygons:\n",
         "  SpatialPoints, SpatialPointsDataFrame,\n",
         "  SpatialPolygons, SpatialPolygonsDataFrame")
  }
  
  match(address, sp$varname)
}