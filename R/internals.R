# ------------------------------------------------------------------------------
# .locate()
#
# Date: 2019-07-08
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
#
# Description:
#   This is an internal function to be used within slice(). It takes a sequence
#   of trips made by a single person (or object) and a numeric vector 
#   representing time at which the location of the person should be extracted.
#   The function returns the results as a data frame object.
#
# Arguments:
#   trip      The input data.frame 'trip' includes the following variables:
#  
#             $tr_id
#             $tr_seq
#             $purpose
#             $mode
#             $o_type
#             $o_time
#             $o_zone
#             $d_type
#             $d_time
#             $d_zone
#
#             It is assumed that the trips in 'trip' are sorted in ascending 
#             order of $o_time. If not, the function may not work as intended.
#
#   at        A numeric vector of length one, indicating the time at which the
#             location of the person should be extracted.
#
#   na.rm     Logical.   
#
#   expand    Logical.
#
#
# ------------------------------------------------------------------------------
.locate <- function(trip, at, na.rm = TRUE, expand = TRUE) {

  n <- nrow(trip)
  begTime <- trip$o_time
  endTime <- trip$d_time
  NAs <- (is.na(begTime) | is.na(endTime))
  
  # ----------------------------------------------------------------------------
  # Handling NA values
  # ----------------------------------------------------------------------------
  if (sum(NAs) >= 1) {
    if (na.rm)
      warning(sum(NAs), "rows removed due to NA", call. = FALSE)
    else
      stop("NAs are not allowed when 'na.rm' is TRUE", call. = FALSE)
    
    # --------------------------------------------------------------------------
    # Update the input data frame, n, begTime and endTime
    # --------------------------------------------------------------------------
    trip <- trip[-which(NAs),]
    n <- nrow(trip)
    begTime <- trip$o_time
    endTime <- trip$d_time
  }
  
  # ----------------------------------------------------------------------------
  # If there is no trip information:
  # ----------------------------------------------------------------------------
  if (n == 0) {
    warning("'trip' has no records; returns NAs")
    output <- data.frame(area = NA, purpose = NA, mode = NA, on.move = NA)
  } 
  
  # ----------------------------------------------------------------------------
  # If there is trip information:
  # ----------------------------------------------------------------------------
  else {
    
    # --------------------------------------------------------------------------
    # Validate the user input arguments
    # --------------------------------------------------------------------------
    if (any(!is.numeric(trip$o_time)))
      stop("invalid 'o_time' values in the object 'trip'", call. = FALSE)
    else if (any(!is.numeric(trip$d_time)))
      stop("invalid 'd_time' values in the object 'trip'", call. = FALSE)

    # --------------------------------------------------------------------------
    # Four possible cases!
    #
    # --------------------------------------------------------------------------
    # <-----> (begTime[i], endTime[i]) <--------> (begTime[n], endTime[n]) <--->
    # --------------------------------------------------------------------------
    # Case 1: at > begTime[i] & at <= endTime[i]
    #         o_zone[i] / mode[i] / purpose[i] / on.move = TRUE         
    #
    # Case 2: at <= begTime[1]
    #         o_zone[1] / NA / NA / on_move = FALSE
    #
    # Case 3: at > endTime[n]
    #         d_zone[n] / mode[n] / purpose[n] / on.move = TRUE
    #
    # Case 4: at > endTime[i] & at <= begTime[i+1]
    #         d_zone[i] / purpose[i] / mode[i] / on_move = FALSE
    # --------------------------------------------------------------------------
    case1 <- (begTime < at) & (endTime >= at)
    
    if (!expand) {
      case2 <- FALSE
      case3 <- FALSE
      case4 <- FALSE
    } else {
      case2 <- begTime[1] >= at
      case3 <- endTime[n] < at
      
      if (n == 1)
        case4 <- FALSE
      else
        case4 <- (endTime[1:(n-1)] < at) & (begTime[2:n] >= at)    
    }
    
    # --------------------------------------------------------------------------
    # Case 1: If the person is in the middle of a trip (or trips), he/she is
    #         assumed to be located at the origin (i.e., trip$o_zone[POSITION]).
    # --------------------------------------------------------------------------
    if (any(case1)) {
      POSITION <- which(case1)
      
      if (length(POSITION) > 1) {
        warning("multiple records match for the given time instance 'at'", 
                call. = FALSE)
        warning("the records other than the first one will be ignored", 
                call. = FALSE)
        POSITION <- POSITION[1]
      }
      
      area <- trip$o_zone[POSITION]
      mode <- trip$mode[POSITION]
      purpose <- trip$purpose[POSITION]
      on.move <- TRUE
    } 
    
    # --------------------------------------------------------------------------
    # Case 2: If the given time instant 'at' is before the first trip is made, 
    #         the person would be located in the origin (i.e., trip$o_zone[1]).
    # --------------------------------------------------------------------------
    else if (case2) {
      area <- trip$o_zone[1]
      mode <- NA
      purpose <- NA
      on.move <- FALSE
    }
    
    # --------------------------------------------------------------------------
    # Case 3: If 'at' is after the last trip is complete, the person is assumed
    #         to be located in his/her final destination (i.e., trip$d_zone[n]).
    # --------------------------------------------------------------------------
    else if (case3) {
      area <- trip$d_zone[n]
      mode <- trip$mode[n]
      purpose <- trip$purpose[n]
      on.move <- FALSE
    }
    
    # --------------------------------------------------------------------------
    # Case 4: If the person is in between two trips, he/she would be located in
    #         the destination of the former (or the origin of the latter). That
    #         is, trip$d_zone[POSITION].
    # --------------------------------------------------------------------------
    else if (any(case4)) {
      POSITION <- which(case4)
      
      if (length(POSITION) > 1)
        stop("unable to find the location at the given time instant 'at'", 
             call. = FALSE)
      
      area <- trip$d_zone[POSITION]
      mode <- trip$mode[POSITION]
      purpose <- trip$purpose[POSITION]
      on.move <- FALSE
    }
    
    # --------------------------------------------------------------------------
    # If none of the four cases are TRUE:
    # --------------------------------------------------------------------------
    else {
      stop("unable to find the location at the given time instant 'at'", 
           call. = FALSE)
    }  
    
    output <- data.frame(area, purpose, mode, on.move, 
                         stringsAsFactors = FALSE)
  }
  
  return(output)
}

# ------------------------------------------------------------------------------
# .get_xy()
#
# Date: 2019-08-20
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
.get_xy <- function(address, sp, varname, verbose = FALSE) {
  
  if (inherits(sp, "SpatialPolygons")) {
    if (verbose) 
      warning("SpatialPolygons or one that inherits from it is provided.\n",
              "The output coordinates are approximate only.", call. = FALSE)
    xy <- coordinates(sp)
  } 
  
  else if (inherits(sp, "SpatialPoints")) {
    xy <- coordinates(sp)
  } 
  
  else {
    stop("'sp' must be one of the following polygons:\n",
         "  SpatialPoints, SpatialPointsDataFrame,\n",
         "  SpatialPolygons, SpatialPolygonsDataFrame")
  }
  
  matching_table <- data.frame(sp)[, names(sp) == varname]
  ID <- match(address, matching_table)
  
  return(xy[ID,])
}