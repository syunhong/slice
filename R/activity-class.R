# ------------------------------------------------------------------------------
# Class 'Personal'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass("Personal", slots = c(hid = "character", pid = "numeric", 
                               famrel = "numeric", 
                               yrborn = "numeric", sex = "numeric", 
                               area = "character", income = "numeric", 
                               occ = "numeric", emp = "numeric", 
                               hhsize = "numeric", dutype = "numeric", 
                               haslic = "logical", hascar = "logical",
                               tstn = "numeric", tbst = "numeric"))

setMethod("initialize", "Personal", 
          function(.Object, hid, pid, famrel, yrborn, sex, area, income, occ, 
                   emp, hhsize, dutype, haslic, hascar, tstn, tbst, ...) {
            
            .Object <- callNextMethod(.Object, ...)
            
            # [[1-1]] Household ID
            if (missing(hid))
              .Object@hid <- character()
            else
              .Object@hid <- hid
            
            # [[1-2]] Personal ID
            if (missing(pid))
              .Object@pid <- numeric()
            else
              .Object@pid <- pid
            
            # [[2]] Relationship to family
            # https://cps.ipums.org/cps-action/variables/FAMREL#codes_section
            if (missing(famrel))
              .Object@famrel <- numeric()
            else
              .Object@famrel <- famrel
            
            # [[3]] Birth year
            if (missing(yrborn))
              .Object@yrborn <- numeric()
            else
              .Object@yrborn <- yrborn
            
            # [[4]] Sex
            if (missing(sex))
              .Object@sex <- numeric()
            else
              .Object@sex <- sex
            
            # [[5]] Area code
            if (missing(area))
              .Object@area <- character()
            else
              .Object@area <- area
            
            # [[6]] Income
            if (missing(income))
              .Object@income <- numeric()
            else
              .Object@income <- income
            
            # [[7]] Occupation
            if (missing(occ))
              .Object@occ <- numeric()
            else
              .Object@occ <- occ
            
            # [[8]] Employment status
            if (missing(emp))
              .Object@emp <- numeric()
            else
              .Object@emp <- emp
            
            # [[9]] Household size
            if (missing(hhsize))
              .Object@hhsize <- numeric()
            else
              .Object@hhsize <- hhsize
            
            # [[10]] Dwelling unit type
            if (missing(dutype))
              .Object@dutype <- numeric()
            else
              .Object@dutype <- dutype
            
            # [[11]] Do you have a driver's license?
            if (missing(haslic))
              .Object@haslic <- logical()
            else
              .Object@haslic <- haslic
            
            # [[12]] Do you have a car?
            if (missing(hascar))
              .Object@hascar <- logical()
            else
              .Object@hascar <- hascar
            
            # [[13]] Time to the nearest subway station
            if (missing(tstn))
              .Object@tstn <- numeric()
            else
              .Object@tstn <- tstn
            
            # [[14]] Time to the nearest bus stop
            if (missing(tbst))
              .Object@tbst <- numeric()
            else
              .Object@tbst <- tbst
            
            .Object
          })

validityPersonal <- function(object) {
  
  if (object@yrborn < 0)
    paste("'yrborn' must be greater than 0")
  else if (object@hhsize < 0)
    paste("'hhsize' must be greater than 0")
  else if (!is.logical(object@haslic))
    paste("'haslic' must be logical")
  else if (!is.logical(object@hascar))
    paste("'hascar' must be logical")
  else if (object@tstn < 0)
    paste("'tstn' must be greater than 0")
  else if (object@tbst < 0)
    paste("'tbst' must be greater than 0")
  else
    TRUE
}

validityPersonal <- function(object) {
  TRUE
}

setValidity("Personal", validityPersonal)


# ------------------------------------------------------------------------------
# Class 'AS'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass("AS", slots = c(info = "Personal", trip = "data.frame"))

setMethod("initialize", "AS", 
          function(.Object, info, trip, ...) {
            
            .Object <- callNextMethod(.Object, ...)
            
            fn <- c("tr_seq", "purpose", "mode", "o_type", "o_time", "o_zone", 
                    "d_type", "d_time", "d_zone")
            
            if (missing(info))
              .Object@info <- new("Personal")
            else if (class(info) == "Personal" & validObject(info))
              .Object@info <- info
            else
              stop("invalid 'info' object", call. = FALSE)
            
            if (missing(trip))
              .Object@trip <- data.frame(tr_seq = NA, purpose = NA, mode = NA,
                                         o_type = NA, o_time = NA, o_zone = NA,
                                         d_type = NA, d_time = NA, d_zone = NA)
            else if (is.data.frame(trip) & all(names(trip) == fn))
              .Object@trip <- trip
            else
              stop("'trip' is not structured as required", call. = FALSE)
            
            .Object
          })

validityAS <- function(object) {
  
}

setValidity("AS", validityAS)


# ------------------------------------------------------------------------------
# Class 'ASL'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass("ASL", slots = c(people = "list"))

setMethod("initialize", "ASL", 
          function(.Object, people, ...) {
            
            .Object <- callNextMethod(.Object, ...)
            
            if (missing(people))
              .Object@people <- list(new("AS"))
            else if (is.list(people))
              .Object@people <- people
            else
              stop("'people' must be of class list", call. = FALSE)
            
            .Object
          })

validityASL <- function(object) {
  
  if (all(lapply(x@people, function(z) class(z)) == "AS"))
    TRUE
  else
    paste("all elements in the 'people' slot must be of class AS")
}

setValidity("ASL", validityASL)
