#' Convert ASPaces to Data Frame
#' @export
#' @description Coerces the Input Object of Class ASpaces to a Data Frame Object
#' @usage as.data.frame.ASpaces(ASpaces, remove.notrip = TRUE)
#'        
#' @param ASpaces an object of class \code{ASpaces}
#' @param remove.notrip logical. See details. 
#' @return An object of class data.frame
#' @details This conversion function converts the ASpaces class composed of a 
#' relatively complex structure into a simple data.frame. The existing ASpaces 
#' class is divided into info and trips inside the data and is stored in a list 
#' form for each individual, so it is easy to distinguish between data but has 
#' a limitation that it is difficult to see at a glance.
#' 
#' This function converts to data.frame to make it easier to check. 
#' In this process, the conversion is performed in a way that the information 
#' of the corresponding info for each trip is connected equally.
#' 
#' Among the arguments, remove.notrip determines whether to keep the trip as 
#' it is or to delete it for an individual that does not exist. If set to TRUE, 
#' which is the default value, the relevant individuals are deleted, and if set 
#' to FALSE, the trip data of NA is attached to the information of personal info.
#' @author Changlock Choi (hihi7100@khu.ac.kr), Seong-Yun Hong (syhong@khu.ac.kr)
#' @examples
#' #load data
#' data(slicedata)
#' 
#' #convert ASpaces into data.frame
#' df.slicedata <- as.data.frame.ASpaces(slicedata)
#' 
#' #compare with original data and result
#' slicedata@data[[1]]
#' df.slicedata[1:2, ]

as.data.frame.ASpaces <- function(ASpaces, remove.notrip = TRUE) {
  
  df <- slot(ASpaces, "data")
  
  ID <- which(sapply(df, function(z) nrow(z@trip)) == 0)
  
  if (length(which(sapply(df, function(z) nrow(z@trip)) == 0)) > 0) {
    warning("the following elements have no trips:\n", 
            paste(ID, collapse = " "), "\n", call. = FALSE)    
  }
  
  if (remove.notrip) {
    if(length(ID) == 0){
      df <- df} else {
        df <- df[-ID]
      }
  } else {
    df[ID] <- lapply(df[ID], function(z) { z@trip[1,] <- NA; return(z) })
  }
  
  output <- lapply(df, function(z) cbind(z@info, z@trip))
  output <- do.call(rbind, output)
  
  return(output)
}
