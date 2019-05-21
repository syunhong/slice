# ------------------------------------------------------------------------------
# Methods for class 'activity'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setMethod("[[", 
          signature = "ASL", 
          definition = function(x, i) { 
            x@people[[i]] 
          })

setMethod("[[<-", 
          signature = c(x = "ASL"),
          definition = function(x, i, value) { 
            x@people[[i]] <- value
            x
          })

setMethod("show", 
          signature = "ASL", 
          definition = function(object) {
            cat("Activity space data for measuring segregation")
          })
