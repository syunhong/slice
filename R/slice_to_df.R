# ------------------------------------------------------------------------------
# slice_to_df()
#
# Date: 2018-08-21
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
slice_to_df <- function(area, by) {
  
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