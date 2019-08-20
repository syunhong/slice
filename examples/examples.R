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

dist900 <- slice(weekdays10.sp@data, at = 900)

as.segdata <- function(area, by) {
  
  if (missing(by)){
    tb <- table(area)
    output <- data.frame(table(area))
    names(output) <- c("area", "count")
  } else {
    tb <- table(area, by)
    segdata.df <- data.frame(rbind(tb))
    colnames(segdata.df) <- colnames(tb)
    output <- cbind(area = rownames(segdata.df), segdata.df)
    rownames(output) <- 1:nrow(output)
  }
    
  return(output)
}

x <- as.segdata(tmp$area, tmp$income)


