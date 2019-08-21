# ------------------------------------------------------------------------------
# Examples
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
library(sp)
library(rgdal)

source("R/ASpace-class.R")
source("R/ASpaces-class.R")
source("R/ASpaces-methods.R")

source("R/internals.R")
source("R/slice.R")

load("d:/dev/slice_data/weekdays10.RData")
load("d:/dev/slice_data/seoul_sp.RData")
# seoul.sp <- readOGR("d:/dev/slice_data/seoul.shp", stringsAsFactors = FALSE,
#                     encoding = "windows-949")

weekdays10.sp <- update(weekdays10, sp = seoul.sp)

dist_0900 <- slice(weekdays10.sp@data, at = 900)
dist_1300 <- slice(weekdays10.sp@data, at = 1300)
dist_1800 <- slice(weekdays10.sp@data, at = 1800)



x <- as.segdata(tmp$area, tmp$income)
weekdays10.subset <- update(weekdays10.sp, data = weekdays10.sp@data[1:100])

