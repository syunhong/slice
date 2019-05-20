# ------------------------------------------------------------------------------
# Class 'activity'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass(Class = "activity", slots = c(people = "list"))

activity <- function(people) {
  new("activity", people = list(list(info = list(), trip = data.frame())))
}
