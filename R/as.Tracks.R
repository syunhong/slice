# ------------------------------------------------------------------------------
# as.Tracks()
#
# Coerces the Input Object of Class ASpace to a Tracks Object
#
# Description:
# 
# 
# 
# Usage:
#
#   as.Tracks(x, sp, varname)
#
# Arguments:
# 
#   x             an object of class ASpace
#
#   sp            an object of class sp used in x
#
#   varname       name of the column in sp's data.frame which can be matched 
#                 with o_zone and d_zone in ASpaces
#
# Details:
# 
# Value:
#
#   An object of class Tracks
#
# Author(s):
#
#   Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
as.Tracks <- function(x, sp, varname) {
  
  # ----------------------------------------------------------------------------
  # (1) Validate the input data 'x'
  # ----------------------------------------------------------------------------
  if (class(x) != "ASpace")
    stop("'x' is not an object of class ASpace", call. = FALSE)
  if (!validateASpace(x))
    stop("'x' is not a valid ASpace object", call. = FALSE)
  
  # ----------------------------------------------------------------------------
  # (2) If 'x' is a valid ASpace object, extracts the slots 'info' and 'trip',
  #     and save them as separate objects for convenience.
  #
  #     If 'trip' has no records, it is not possible (and does not make sense)
  #     to transform the input object to 'Tracks'. The function stops here.
  #
  #     If $o_time and $d_time in 'trip' (i.e., departure time at the origin
  #     and arrival time at the destination) are NOT of class POSIXct, gives
  #     a warning to users and coerces the values to POSIXct.
  # ----------------------------------------------------------------------------
  info <- data.frame(x@info, stringsAsFactors = FALSE)
  trip <- x@trip

  if (nrow(trip) < 1)
    stop("coercion failed: slot 'trip' is empty")
  
  if (class(trip$o_time) != "POSIXct") {
    warning("$o_time is not of class POSIXct:\n",
            "time information may not be accurate", call. = FALSE)
    trip$o_time <- as.POSIXct(Sys.Date()) + trip$o_time 
  }
  
  if (class(trip$d_time) != "POSIXct") {
    warning("$d_time is not of class POSIXct:\n",
            "time information may not be accurate", call. = FALSE)
    trip$d_time <- as.POSIXct(Sys.Date()) + trip$d_time
  }
  
  # ----------------------------------------------------------------------------
  # (3) Trips with the same $tr_id will be considered parts of a single 'Track'
  #     object. For example, if 'trip' is:
  #
  #       tr_id tr_seq ...
  #     1     1      1 ...
  #     2     1      2 ...
  #     3     2      1 ...
  #     4     2      2 ...
  #     5     2      3 ...
  #     6     3      1 ...
  #
  #     the function as.Tracks() will return an object of class 'Tracks' that 
  #     contains three 'Track' objects.
  #
  #     A for loop is used here now (2019-08-22). There might be a better and
  #     faster way, but it requires more thoughts. 
  #
  #     LET'S JUST LEAVE IT AS IT IS FOR NOW AND COME BACK LATER.
  # ----------------------------------------------------------------------------
  tracks <- list()
  
  tr_id <- unique(trip$tr_id)
  for (i in tr_id) {
    CURRENT <- trip$tr_id == i
    
    track <- trip[CURRENT,]
    
    track_xy <- rbind(.get_xy(track$o_zone, sp, varname),
                      .get_xy(track$d_zone, sp, varname))
    rownames(track_xy) <- 1:nrow(track_xy)
    
    if (any(is.na(track_xy)))
      stop("coercion failed: NAs produced by .get_xy()")

    track_time <- c(track$o_time, track$d_time)
    track_data <- data.frame(type = c(track$o_type, track$d_type))
    track_data <- cbind(info, track_data)
    
    tracks[[i]] <- Track(STIDF(SpatialPoints(track_xy, sp@proj4string), 
                               track_time, track_data))
  }
  
  return(Tracks(tracks))
}
