#' Class ASpaces
#' @exportClass ASpaces
#' @include ASpaces-class.R
#' @description A S4 class for storing activity-space data; Contains information of one individual person
#' @slot data an object of list containing ASpace object.
#' @slot attr an object of list containing metadata.
#' @slot sp an object of spatialpolygon or spatialpoint class data.
#' @details
#' The slot 'data' holds a list of ASpace objects (i.e., activity
#' space information for an individual).
#' 
#' The slot 'attr' has supplementary information about the data. For
#' example, it may contain the name of the survey, the date it was
#' conducted, or the code book for the variables.
#' 
#' The slot 'sp' contains spatialpolygon or spatialpoint class data
#' that can indicate the specific location of o_zone and d_zone in 
#' trip.
#' @seealso \linkS4class{ASpace} 
#' @author Seong-Yun Hong (syhong@khu.ac.kr)
#' @examples
#'#example of creating ASpace class
#'#creates sample info data
#'testinfo <- list()
#'
#'#creates sample trip data
#'testtrip <- data.frame(tr_id = 1:10, tr_seq = rep(NA, 10), purpose = rep(NA, 10),
#'                       mode = rep(NA, 10), o_type = rep(NA, 10), 
#'                     o_time = c(200,300,400,500,600,700,800,900,1000,1100),
#'                     o_zone = c(1,2,3,4,5,6,5,4,3,2),
#'                     d_type = rep(NA, 10),
#'                     d_time = c(300,400,500,600,700,800,900,1000,1100,1200),
#'                     d_zone = c(2,3,4,5,6,5,4,3,2,1))
#'                     
#'#constructs an object of class 'ASpace'
#' testASpace <- new("ASpace", info = list(id = 1), trip = testtrip)
#'
#'#constructs an object of class 'ASpaces'
#' testtASpaces <- new("ASpaces", data = list(testaspace), sp = testshp)

setClass("ASpaces", slots = c(data = "list", attr = "list", sp = "Spatial"))

setMethod("initialize", "ASpaces", 
          function(.Object, data, attr, sp, ...) {
            
            .Object <- callNextMethod(.Object, ...)
            
            if (missing(data))
              .Object@data <- list(new("ASpace"))
            else if (is.list(data))
              .Object@data <- data
            else
              stop("'data' must be of class list", call. = FALSE)

            attr.names <- c("name", "date", "desc")
            if (missing(attr))
              .Object@attr <- list(name = character(),
                                   date = Sys.time(),
                                   desc = character())
            else if (is.list(attr) & (attr.names %in% names(attr)))
              .Object@attr <- attr
            else
              stop("'attr' must be of class list", call. = FALSE)

            if (missing(sp))
              .Object@sp <- new("Spatial")
            else
              .Object@sp <- sp

            .Object
          })

validateASpaces <- function(object) {
  
  if (all(lapply(object@data, function(z) class(z)) == "ASpace"))
    TRUE
  else
    paste("all elements in the 'data' slot must be of class ASpace")
}

setValidity("ASpaces", validateASpaces)
