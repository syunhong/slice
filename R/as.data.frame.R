# ------------------------------------------------------------------------------
# as.data.frame()
#
# Date: 2018-08-21
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
as.data.frame <- function(ASpaces, remove.notrip = TRUE) {
  
  df <- slot(ASpaces, "data")
  
  ID <- which(sapply(df, function(z) nrow(z@trip)) == 0)
  
  if (length(ID) > 0) {
    warning("the following elements have no trips:\n", 
            paste(ID, collapse = " "), "\n", call. = FALSE)    
  }

  if (remove.notrip) {
    df <- df[-ID]
  } else {
    df[ID] <- lapply(df[ID], function(z) { z@trip[1,] <- NA; return(z) })
  }
  
  output <- lapply(df, function(z) cbind(z@info, z@trip))
  output <- do.call(rbind, output)
  
  return(output)
}
