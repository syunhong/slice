#' Find Individual's Location by Time
#' @export
#' @description Find where an individual is located at a specific time during continuous trip
#' @usage .locate(trip, id, at, na.rm, silent)
#' @param trip an object of data.frame containg person's continuous trip.
#' @param id a vector of length 1, indicating person's ID
#' @param at a numeric vector of length 1, specifying the time at which 
#'           the location of an individual needs to be extracted 
#' @param silent logical. If TRUE, if any trip records have NAs in the time
#'                 fields, a warning message will be provided.
#' 
#' @return a single row data.frame containing correspond with 'at'
#' @seealso \code{\link{slice}}
#' @author Seong-Yun Hong (syhong@khu.ac.kr)
#' @examples 
#'#creates sample trip data
#'testtrip <- data.frame(tr_id = 1:10, tr_seq = rep(NA, 10), purpose = rep(NA, 10),
#'                       mode = rep(NA, 10), o_type = rep(NA, 10), 
#'                     o_time = c(200,300,400,500,600,700,800,900,1000,1100),
#'                     o_zone = c(1,2,3,4,5,6,5,4,3,2),
#'                     d_type = rep(NA, 10),
#'                     d_time = c(300,400,500,600,700,800,900,1000,1100,1200),
#'                     d_zone = c(2,3,4,5,6,5,4,3,2,1))
#'                     
#'#Find where at 800 via .locate
#'.locate(testtrip, 1, 600, na.rm = TRUE, silent = FALSE)

.locate <- function(trip, id, at, na.rm, silent) {

  n <- nrow(trip)
  begTime <- as.numeric(trip$o_time)
  endTime <- as.numeric(trip$d_time)
  NAs <- (is.na(begTime) | is.na(endTime))
  
  # ----------------------------------------------------------------------------
  # Handling NA values
  # ----------------------------------------------------------------------------
  if (any(NAs) & na.rm) {
    trip <- trip[-which(NAs),]
    n <- nrow(trip)
    begTime <- as.numeric(trip$o_time)
    endTime <- as.numeric(trip$d_time)
    
    if (!silent)
      warning("[", id, "] ", sum(NAs), " rows removed due to NA", 
              call. = FALSE)
  } else if (any(NAs) & !na.rm) {
    stop("NAs in 'o_time' or 'd_time' are not allowed when na.rm = FALSE", 
         call. = FALSE)
  }
  
  # ----------------------------------------------------------------------------
  # If there is no trip information:
  # ----------------------------------------------------------------------------
  if (n == 0) {
    if (!silent)
      warning("[", id, "] 'trip' has no records; returns NAs")
    output <- data.frame(location = NA, purpose = NA, mode = NA, on.move = NA)
  }
  
  # ----------------------------------------------------------------------------
  # If there is trip information:
  # ----------------------------------------------------------------------------
  else {
    # --------------------------------------------------------------------------
    # Validate the user input arguments
    # --------------------------------------------------------------------------
    if (any(!is.numeric(as.numeric(trip$d_time))))
      stop("invalid 'o_time' values in the object 'trip'", call. = FALSE)
    else if (any(!is.numeric(as.numeric(trip$d_time))))
      stop("invalid 'd_time' values in the object 'trip'", call. = FALSE)

    # --------------------------------------------------------------------------
    # Four possible cases!
    #
    # --------------------------------------------------------------------------
    # <-----> (begTime[i], endTime[i]) <--------> (begTime[n], endTime[n]) <--->
    # --------------------------------------------------------------------------
    # Case 1: at <= begTime[i]
    #         o_zone[i] / NA / NA / on_move = FALSE
    #
    # Case 2: at > endTime[n]
    #         d_zone[n] / mode[n] / purpose[n] / on.move = FALSE
    #
    # Case 3: at > begTime[i] & at <= endTime[i]
    #         o_zone[i] / mode[i] / purpose[i] / on.move = TRUE         
    #
    # Case 4: at > endTime[i] & at <= begTime[i+1]
    #         d_zone[i] / purpose[i] / mode[i] / on_move = FALSE
    # --------------------------------------------------------------------------
    case1 <- begTime[1] >= at
    case2 <- endTime[n] < at
    case3 <- (begTime < at) & (endTime >= at)
    
    if (n < 2)
      case4 <- FALSE
    else
      case4 <- (endTime[1:(n-1)] < at) & (begTime[2:n] >= at)     

    # --------------------------------------------------------------------------
    # Case 1: If the given time instant 'at' is before the first trip is made, 
    #         the person would be located in the origin (i.e., trip$o_zone[1]).
    # --------------------------------------------------------------------------
    if (case1) {
      location <- trip$o_zone[1]
      mode <- NA
      purpose <- NA
      on.move <- FALSE
    }
    
    # --------------------------------------------------------------------------
    # Case 2: If 'at' is after the last trip is complete, the person is assumed
    #         to be located in his/her final destination (i.e., trip$d_zone[n]).
    # --------------------------------------------------------------------------
    else if (case2) {
      location <- trip$d_zone[n]
      mode <- trip$mode[n]
      purpose <- trip$purpose[n]
      on.move <- FALSE
    }
    
    # --------------------------------------------------------------------------
    # Case 3: If the person is in the middle of a trip (or trips), he/she is
    #         assumed to be located at the origin (i.e., trip$o_zone[POSITION]).
    # --------------------------------------------------------------------------
    else if (any(case3)) {
      POSITION <- which(case3)
      
      # ------------------------------------------------------------------------
      # EXCEPTIONS: What if there are multiple matches (i.e., TRUE) for case 3?
      # Use the first match? Or generate an error and stop the process?
      # This part of the code defines what to do when such error occurs.
      # ------------------------------------------------------------------------
      if (length(POSITION) > 1) {
        msg <- "multiple records match for the given time instance 'at'"
        msg <- paste(msg, 
                     "the records other than the first one will be ignored",
                     sep = "\n")
        warning("[", id, "] case 3: ", msg, call. = FALSE)
        POSITION <- POSITION[1]
      }

      location <- trip$o_zone[POSITION]
      mode <- trip$mode[POSITION]
      purpose <- trip$purpose[POSITION]
      on.move <- TRUE
    } 
    
    # --------------------------------------------------------------------------
    # Case 4: If the person is in between two trips, he/she would be located in
    #         the destination of the former (or the origin of the latter). That
    #         is, trip$d_zone[POSITION].
    # --------------------------------------------------------------------------
    else if (any(case4)) {
      POSITION <- which(case4)
 
      # ------------------------------------------------------------------------
      # EXCEPTIONS: What if there are multiple matches (i.e., TRUE) for case 4?
      # Use the first match? Or generate an error and stop the process?
      # This part of the code defines what to do when such error occurs.
      # ------------------------------------------------------------------------
      if (length(POSITION) > 1) {
        msg <- "multiple records match for the given time instance 'at'"
        msg <- paste(msg, 
                     "the records other than the first one will be ignored",
                     sep = "\n")
        warning("[", id, "] case 4: ", msg, call. = FALSE)
        POSITION <- POSITION[1]      
      }

      location <- trip$d_zone[POSITION]
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
    
    output <- data.frame(location, purpose, mode, on.move, 
                         stringsAsFactors = FALSE)
  }
  
  return(output)
}
