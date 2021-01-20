#' Convert Objects to Class tracksCollection
#' @export
#' @description Coerces the Input Object of Class ASpaces to a tracksCollection Object
#' @usage as.tracksCollection(x, sp, varname)
#'        
#' @param x an object of class ASpaces
#' @param sp an object of class sp used in x
#' @param varname name of the column in sp's data.frame which can be matched 
#'             with o_zone and d_zone in ASpaces
#' @return An object of class tracksCollection
#' @details Function \code{as.trackCollection} accepts converts ASpaces objects 
#' to an onject of class \code{tracksCollection}. In the conversion since the 
#' individual's travel position matches the position on the spatial data, care 
#' must be taken in selecting 'sp'. If 'sp' is not given, the 'sp' contained in 
#' \code{ASpaces} is selected.
#' 
#' 
#' 
#' @seealso \code{\link{as.Tracks}}
#' @author Seong-Yun Hong (syhong@khu.ac.kr)
#' @examples 
#' ## Not run:
#' # creates sample info data
#' testinfo <- list()
#'
#' # creates sample trip data
#' testtrip <- data.frame(tr_id = 1:10, tr_seq = rep(NA, 10), purpose = rep(NA, 10),
#'                       mode = rep(NA, 10), o_type = rep(NA, 10), 
#'                       o_time = c(200,300,400,500,600,700,800,900,1000,1100),
#'                       o_zone = c(1,2,3,4,5,6,5,4,3,2),
#'                       d_type = rep(NA, 10),
#'                       d_time = c(300,400,500,600,700,800,900,1000,1100,1200),
#'                       d_zone = c(2,3,4,5,6,5,4,3,2,1))
#'                     
#'#constructs an object of class 'ASpace'
#' testASpace <- new("ASpace", info = list(id = 1), trip = testtrip)
#'
#'#constructs an object of class 'ASpaces'
#' testtASpaces <- new("ASpaces", data = list(testaspace), sp = testshp)
#' 
#' testTC <- as.tracksCollection(testASpaces, varname = "adm_dr_cd")
as.tracksCollection <- function(x, sp, varname) {

  # ----------------------------------------------------------------------------
  # If 'varname' is not given, stop the function
  # ----------------------------------------------------------------------------
  if (missing(varname))
    stop("'varname' must be provided", call. = FALSE)
  
  # ----------------------------------------------------------------------------
  # If 'sp' is missing, take the sp from given ASpaces "x"
  # ----------------------------------------------------------------------------
  if (missing(sp)){
    sp <- x@sp
  warning("Since sp is not given, the spatial data in x is used", 
          call. = FALSE)
  }
  
  tracks_list <- lapply(x@data, function(z) try(as.Tracks(z, sp, varname), TRUE))
  errorID <- sapply(tracks_list, function(z) class(z) == "try-error")
  
  return(list(tracks_list, errorID))
}

