# ------------------------------------------------------------------------------
# Methods for class 'activity'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setMethod("[[", 
          signature = "activity", 
          definition = function(x, i) { 
            x@people[[i]] 
          })

setMethod("show", 
          signature = "activity", 
          definition = function(object) {
            cat("Activity space data for measuring segregation")
          })
