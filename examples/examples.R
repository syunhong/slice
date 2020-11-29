# ------------------------------------------------------------------------------
# Examples
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
library(sp)
library(rgdal)
library(trajectories)
library(spacetime)

source("R/as.Tracks.R")
source("R/as.TracksCollection.R")
source("R/ASpace-class.R")
source("R/ASpaces-class.R")
source("R/ASpaces-methods.R")
source("R/get_xy.R")
source("R/locate.R")
source("R/locate.R")
source("R/select.R")
source("R/slice.R")
source("R/slice2df.R")
source("R/subset.R")

load("examples/sampledata.RData")

##example of simple slice function
slice(testASP, 1200)

##example of as.TracksCollection
testTC <- as.tracksCollection(testASP) ##without any argument

testTC <- as.tracksCollection(testASP, varname = "adm_dr_cd") ##without sp

testTC <- as.tracksCollection(testASP, testASP@sp, varname = "adm_dr_cd")

##example of as.TracksCollection with point data
testtrip <- data.frame(tr_id = 1:10, tr_seq = rep(NA, 10), purpose = rep(NA, 10),
                      mode = rep(NA, 10), o_type = rep(NA, 10), 
                      o_time = c(200,300,400,500,600,700,800,900,1000,1100),
                      o_zone = c(1,2,3,4,5,6,5,4,3,2),
                      d_type = rep(NA, 10),
                      d_time = c(300,400,500,600,700,800,900,1000,1100,1200),
                      d_zone = c(2,3,4,5,6,5,4,3,2,1))

testaspace <- new("ASpace", info = list(id = 1), trip = testtrip)

pointaspaces <- new("ASpaces", data = list(testaspace), sp = testshp)

as.tracksCollection(pointaspaces, varname = "adress")
