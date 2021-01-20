#' generic method for class ASpaces
#' @export
#' @docType methods
#' @rdname ASpaces-methods

# ------------------------------------------------------------------------------
# Methods for class "ASpaces"
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
setMethod("[[", 
          signature = "ASpaces", 
          definition = function(x, i) { 
            x@data[[i]] 
          })
#' generic method for class ASpaces
#' @export
setMethod("[[<-", 
          signature = c(x = "ASpaces"),
          definition = function(x, i, value) { 
            x@data[[i]] <- value
            x
          })
#' generic method for class ASpaces
#' @export
setMethod("update", 
          signature = "ASpaces",
          definition = function(object, data, attr, sp) {
            
            if (!missing(data)) {
              object@data <- data
            } 
            
            if (!missing(attr)) {
              object@attr <- attr
            }
            
            if (!missing(sp)) {
              object@sp <- sp
            }
          
            return(object)
          }
          )
#' generic method for class ASpaces
#' @export
setMethod("summary", 
          signature = "ASpaces", 
          definition = function(object) {
            trips <- unlist(lapply(object@data, function(z) nrow(z@trip)))

            cat("An object of class \"ASpaces\"\n\n")
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
            cat(class(object@sp)[1], "\n")
          })
#' generic method for class ASpaces
#' @export
setMethod("show", 
          signature = "ASpaces", 
          definition = function(object) {
            cat("An object of class \"ASpaces\"\n")
            cat("Slot \"data\":\n")
            cat(length(object@data), "list elements, each of which is:\n")
            str(object@data[[1]])
            cat("\n")
            
            cat("Slot \"attr\":\n")
            print(object@attr)
            cat("\n")
            
            cat("Slot \"sp\":\n")
            print(object@sp)
          })
