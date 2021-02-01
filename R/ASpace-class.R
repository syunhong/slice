#' Class ASpace
#' @exportClass ASpaces
#' @include ASpace-class.R
#' @description A S4 class for storing activity-space data; Contains information of one individual person
#' @slot info an object of list. representing the demographic characteristic of individual.
#' @slot trip an object of data.frame containing a trip data made by the person 
#' described in the info slot.
#' @details 
#'             
#' The slot 'info' contains demographic and socioeconomic variables
#' of the respondent as a list object. The list object must include
#' the following named element(s):
#'
#' .. $id
#' 
#' If this information is not given by user at the time of creation
#' of 'ASpace', an empty list will be supplied.
#'
#'           
#' The slot 'trip' contains information on trips made by the survey
#' respondent. It a data frame object with the following variables:
#' 
#'.. $tr_id: Trip ID (integer)\cr
#'.. $tr_seq: Sequence ID of the trip (integer)\cr
#'.. $purpose: Purpose of the trip (factor)\cr
#'.. $mode: Mode of the trip (factor)\cr
#'.. $o_type: Type of the origin (factor)\cr
#'.. $o_time: Time departed from the origin (integer, or POSIXct)\cr
#'.. $o_zone: Area code for the origin (character)\cr
#'.. $d_type: Type of the destination (factor)\cr
#'.. $d_time: Time arrived at the destination (integer, or POSIXct)\cr
#'.. $d_zone: Area code for the destination (character)\cr
#'      
#' @seealso \linkS4class{ASpaces} 
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
#' testaspace <- new("ASpace", info = list(id = 1), trip = testtrip)
#' 
#' 

setClass("ASpace", slots = c(info = "list", trip = "data.frame"))

setMethod("initialize", "ASpace", 
          function(.Object, info, trip, ...) {
            
            .Object <- callNextMethod(.Object, ...)

            if (missing(info))
              .Object@info <- list(id = character())
            else {
              essentials <- c("id")
              
              # If 'info' given by user is a valid object:
              if (all(essentials %in% names(info))) 
                .Object@info <- info
              
              # Otherwise:
              else 
                stop("invalid 'info' object", call. = FALSE)
            }

            if (missing(trip))
              .Object@trip <- data.frame(tr_id = 1L, tr_seq = 1L, 
                                         purpose = NA, mode = NA,
                                         o_type = NA, o_time = NA, o_zone = NA,
                                         d_type = NA, d_time = NA, d_zone = NA)
            else {
              
              fn <- c("tr_id", "tr_seq", "purpose", "mode", "o_type", "o_time", 
                      "o_zone", "d_type", "d_time", "d_zone")
              
              # If 'trip' given by user is a valid object:
              if (is.data.frame(trip) & all(names(trip) == fn))
                .Object@trip <- trip

              # Otherwise:
              else
                stop("'trip' is not structured as required", call. = FALSE)
            }

            .Object
          })

validateASpace <- function(object) {

  info <- object@info
  info.vars <- c("id")
  
  trip <- object@trip
  trip.vars <- c("tr_id", "tr_seq", "purpose", "mode", "o_type", "o_time", 
                 "o_zone", "d_type", "d_time", "d_zone")
  
  if (!is.list(info))
    stop("invalid object in 'info'", call. = FALSE)
  else if (!is.data.frame(trip))
    stop("invalid object in 'trip'", call. = FALSE)
  else if (!all(info.vars %in% names(info)))
    stop(paste("one or more of the followings are missing from 'info':",
               "\n", info.vars), call. = FALSE)
  else if (!all(names(trip) == trip.vars))
    stop("'trip' is not structured as required", call. = FALSE)
  
  if (nrow(trip) <= 0)
    warning("no information on trips", call. = FALSE)
  else if (!is.integer(trip[,1]) | min(trip[,1]) != 1)
    stop("invalid values in column 'tr_id', slot 'trip'", call. = FALSE)

  return(TRUE)
}

setValidity("ASpace", validateASpace)