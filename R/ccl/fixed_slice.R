# ------------------------------------------------------------------------------
# slice() fixed
#
# Date : 2019.07.9
# ------------------------------------------------------------------------------
### apply with class "AS"
slice <- function(data, at, attr = c("id", "pid")) {
  
  
  output <- lapply(slot(data, "data"), function(z) slice.person(z, at))
  df.output <- do.call(rbind.data.frame, output)
  attrfixed <- attr
  
  
  if("trip" %in% attr){
    attrfixed <- c(attr, names(data@data[[1]]@trip), "stay")
    attrfixed <- attrfixed[!attrfixed == "trip"]
  }
  df.output <- df.output[order(as.numeric(rownames(df.output))), 
                         c(attrfixed)]

  return(df.output)
}
