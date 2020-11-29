# ------------------------------------------------------------------------------
# Class "ASpaces"
#
# Description:
#
#
# Slots:
#
#   info
#
#   trip
#
#
# Extends:
#
#   Class "ASpace"
#
# Methods:
#
#
#
#
#
#
# Author(s):
#
#   Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
setClass("ASpaces", slots = c(data = "list", attr = "list", sp = "Spatial"))

setMethod("initialize", "ASpaces", 
          function(.Object, data, attr, sp, ...) {
            
            .Object <- callNextMethod(.Object, ...)
            
            # ------------------------------------------------------------------
            # The slot 'data' holds a list of ASpace objects (i.e., activity
            # space information for an individual).
            # ------------------------------------------------------------------
            if (missing(data))
              .Object@data <- list(new("ASpace"))
            else if (is.list(data))
              .Object@data <- data
            else
              stop("'data' must be of class list", call. = FALSE)

            # ------------------------------------------------------------------
            # The slot 'attr' has supplementary information about the data. For
            # example, it may contain the name of the survey, the date it was
            # conducted, or the code book for the variables.
            # ------------------------------------------------------------------
            attr.names <- c("name", "date", "desc")
            if (missing(attr))
              .Object@attr <- list(name = character(),
                                   date = Sys.time(),
                                   desc = character())
            else if (is.list(attr) & (attr.names %in% names(attr)))
              .Object@attr <- attr
            else
              stop("'attr' must be of class list", call. = FALSE)

            # ------------------------------------------------------------------
            # The slot 'sp' contains spatialpolygon or spatialpoint class data
            # that can indicate the specific location of o_zone and d_zone in 
            # trip
            # ------------------------------------------------------------------
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
