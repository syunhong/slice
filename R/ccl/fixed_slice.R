# ------------------------------------------------------------------------------
# slice() fixed
#
# Date : 2019.05.31
# ------------------------------------------------------------------------------
### apply with class "AS"
slice <- function(data, at, attr = c("hid", "pid")) {
  
  
  output <- lapply(slot(data, "people"), function(z) slice.person(z, at))
  df.output <- do.call(rbind.data.frame, output)
  attrfixed <- attr
  
  output <- lapply(slot(data, "people"), function(z) slice.person(z, at))
  df.output <- do.call(rbind.data.frame, output)
  
  if("trip" %in% attr){
    attrfixed <- c(attr, names(data@people[[1]]@trip), "stay")
    attrfixed <- attrfixed[!attrfixed == "trip"]
  }
  df.output <- df.output[order(as.numeric(rownames(df.output))), 
                         c("area", attrfixed)]
  
  df.output[, 1] <- .transh(df.output[, 1])
  
  
  return(df.output)
}