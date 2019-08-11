# ------------------------------------------------------------------------------
# Methods for class 'ASpaces'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setMethod("[[", 
          signature = "ASpaces", 
          definition = function(x, i) { 
            x@data[[i]] 
          })

setMethod("[[<-", 
          signature = c(x = "ASpaces"),
          definition = function(x, i, value) { 
            x@data[[i]] <- value
            x
          })

setMethod("summary", 
          signature = "ASpaces", 
          definition = function(object) {
            trips <- unlist(lapply(weekdays10@data, function(z) nrow(z@trip)))
            
            cat("A data object of class \"ASpaces\"\n\n")
            cat("Name:", object@attr$name, "\n")
            cat("Date:", as.character(object@attr$date), "\n")
            cat("Description:\n")
            cat(object@attr$desc, "\n")
            
            cat("Slot \"data\":\n")
            cat("# elements:", length(object@data), "\n")
            cat("# elements with no trips:", sum(trips == 0), "\n")
            cat("# trips per element:", "\n")
            cat("min\t mean\t max", "\n")
            cat(round(min(trips), 1), "\t", 
                round(mean(trips), 1), "\t", 
                round(max(trips), 1), "\n\n")
            
            cat("Slot \"sp\":\n")
            cat("SpatialPolygonsDataFrame\n")
          })

setMethod("show", 
          signature = "ASpaces", 
          definition = function(object) {
            cat("An object of class \"ASpaces\"\n\n")
            cat("Slot \"data\":\n")
            
            cat("Slot \"data\":\n")
            
            cat("Slot \"sp\":\n")
          })
