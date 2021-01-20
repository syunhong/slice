#' Convert Objects to Class Tracks
#' @export
#' @description Coerces the Input Object of Class ASpaces to a Tracks Object
#' @usage as.Tracks(x, sp, varname)
#'        
#' @param x an object of class ASpace
#' @param sp an object of class sp used in x
#' @param varname name of the column in sp's data.frame which can be matched 
#                 with o_zone and d_zone in ASpaces
#' @return An object of class Tracks
#' @details Function \code{as.Track} accepts converts \code{ASpace} objects 
#' to an onject of class \code{Tracks}. When converting a single \code{ASpace} 
#' object, unlike the \code{ASpaces} class, there is basically no sp given, so 
#' it is essential to enter the sp argument. 
#' 
#' Also, since the input of the time is fixed to the \code{POSIXct} or 
#' \code{POSIXt} class in the \code{Tracks} class, attention is needed that the 
#' existing time format is changed to the type of the corresponding class.
#' 
#' @seealso \code{\link{as.Tracks}}
#' @author Seong-Yun Hong (syhong@khu.ac.kr)
#' @examples 
#' ## Not run:
#' # creates sample info data
#' testinfo <- list()
#'
#' # creates sample trip data
#' testtrip <- data.frame(tr_id = 1:10, tr_seq = rep(NA, 10), purpose = rep(NA, 10),
#'                       mode = rep(NA, 10), o_type = rep(NA, 10), 
#'                       o_time = c(200,300,400,500,600,700,800,900,1000,1100),
#'                       o_zone = c(1,2,3,4,5,6,5,4,3,2),
#'                       d_type = rep(NA, 10),
#'                       d_time = c(300,400,500,600,700,800,900,1000,1100,1200),
#'                       d_zone = c(2,3,4,5,6,5,4,3,2,1))
#'                     
#'#constructs an object of class 'ASpace'
#' testASpace <- new("ASpace", info = list(id = 1), trip = testtrip)
#' 
#' testTracks <- as.tracksCollection(testASpace, varname = "adm_dr_cd")

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
