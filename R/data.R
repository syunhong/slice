#' Syhthetic Patterns of Activity Space
#' 
#' @description A dataset containing the demographic attributes and trip record of almost
#' 100 people
#' @format An object of clss ASpaces. The dataset contains 84 people's demographic attribute and
#' 236 trip records.
#' \describe{
#'   \item{data}{include list object which contains info and trip data}
#'   \item{info}{14 different demographic characteristics, including individual ID}
#'   \item{trip}{10 essential information which is needed to configure ASpace class}
#'   \item{sp}{No sp data is given since it is synthetic travel information}
#'   ...
#' }
#' @examples 
#' #load data
#' data(slicedata)
#' 
#' #validate whether data has normal condition
#' validateASpaces(slicedata)
#' 
#' #check data's basic information
#' summary(slicedata)
#' show(slicedata)
#' @usage data(slicedata)

"slicedata"


