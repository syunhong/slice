#' Retreiving Population Distribution at Given Time
#' @export
#' @description Identifies the location of each individual at the given time 
#'              instant
#' @usage slice(x, at, vars, showProgress = TRUE, na.rm = TRUE, silent = FALSE, 
#' mc = FALSE, core)
#' @param x an object of class ASpaces
#' @param at a numeric vector of length 1, specifying the time at which 
#'           the location of an individual needs to be extracted
#' @param vars an optional character vector. See Details.
#' @param showProgress logical. If TRUE, a progress bar appears on the R console 
#'                 while iterating.
#' @param silent logical. If TRUE, if any trip records have NAs in the time
#'                 fields, a warning message will be provided.
#' @param mc logical. If TRUE, function is operated in parallel. See Details.
#' @param core numeric vector of length 1, specifying the Number of cores to be 
#'            utilized in parallel computation.
#' @return A data.frame
#' @details the \code{slice()} function attempts to retrieve the locations of people 
#'  in the input object 'x' at the given time instant 'at'.
#'
#'  The function looks into the slot 'trip' in each of the elements in the
#'  slot 'data' of the object 'x' (i.e., x@data[[i]]@trip where i = {1 ... n}).
#'
#'  The 'trip' slot contains a sequence of trips made by each person, and it is
#'  assumed that the trips are sorted in ascending order of the departure time
#'  (i.e., $o_time). If NOT, this function may not work as intended.
#'   
#' 'vars' are used to refine the information in the resulting \code{ASpace}'s 
#' \code{info} slot. When a specific character vector is input to 'vars', the 
#' \code{info} slot of \code{ASpace} resulting from the function has only the 
#' input variables. If 'vars' is not input, the result is same with the original 
#' \code{info} slot.
#' 
#'  The 'mc' and 'core' arguments are used to enable parallelization of the 
#'  slice function. If the option of mc is set to TRUE, the function works in 
#'  a multi-core environment. At this time, the number of cores (threads) used 
#'  can be adjusted with the 'core' argument, and if the set number exceeds 
#'  the user's environment, it is adjusted to -1 of the maximum number.
#'  
#'  The result of the slice function is provided in the form of a single 
#'  data.frame, and one variable is added excluding the variables of info and 
#'  trip. For this, a variable called on.move is added, and the variable is 
#'  information about whether a person existing at the location is passing at 
#'  the time specified by the user. If on.move is TRUE, it means that the person 
#'  is moving.
#'  
#' @author Seong-Yun Hong (syhong@khu.ac.kr)
#' @examples
#' 
#' # load data
#' data(slicedata)
#' 
#' # extracting population distribution at 1:00PM
#' slice(slicedata, at = 1300)
#' 
#' # observation of changes in population distribution over time
#' result <- list()
#' time <- seq(800, 2000, 100)
#' for(i in 1:length(time)){
#' result[[i]] <- slice(slicedata, at = time[i])}
#' 
#' # running function with parallelization
#' if (require(parallel)){
#' result <- slice(newtestset, 1200, silent = TRUE, mc = TRUE, core = 2)
#'
#' result <- slice(newtestset, 1200, silent = TRUE, mc = TRUE, core = 6)
#'
#' result <- slice(newtestset, 1200, silent = TRUE, mc = TRUE, core = 100)
#' }


slice <- function(x, at, vars, showProgress = TRUE, na.rm = TRUE, 
                  silent = FALSE, mc = FALSE, core) {
  
  # ----------------------------------------------------------------------------
  # Is 'x' a valid ASpaces object?
  # ----------------------------------------------------------------------------
  if (!inherits(x, "ASpaces"))
    stop("'x' must be of class ASpaces", call. = FALSE)
  else
    df <- slot(x, "data")
  
  # ----------------------------------------------------------------------------
  # If 'at' is not given, stop the function
  # ----------------------------------------------------------------------------
  if (missing(at))
    stop("'at' must be provided", call. = FALSE)
  
  # ----------------------------------------------------------------------------
  # If 'vars' are not specified, return all variables in the input data
  # ----------------------------------------------------------------------------
  if (missing(vars))
    vars <- names(slot(df[[1]], "info"))
  
  # ----------------------------------------------------------------------------
  # If 'mc' is given, check number of core
  # core must be one length integer value and cannot over user's core
  # ----------------------------------------------------------------------------  
  
  if (mc){
    core <- .checkslicemc(core)
    mccl <- makeCluster(core)
    clusterExport(cl = mccl, c(".locate"))
  }
  
  if (mc == TRUE){
    output <- parLapply(cl = mccl, X = df, fun = function(z, vars){
      info <- as.data.frame(z@info, stringsAsFactors = FALSE)
      trip <- .locate(z@trip, info$id, at, na.rm, silent)
      output <- cbind(info[vars], trip)
      return(output)
    
    })
    stopCluster(mccl)
  } else if(showProgress & require(pbapply)){
    output <- pblapply(df, FUN = function(z, vars) {
      info <- as.data.frame(z@info, stringsAsFactors = FALSE)
      trip <- .locate(z@trip, info$id, at, na.rm, silent)
      output <- cbind(info[vars], trip)
      return(output)
    
    })
    
  } else
    output <- lapply(df, FUN = function(z, vars) {
      info <- as.data.frame(z@info, stringsAsFactors = FALSE)
      trip <- .locate(z@trip, info$id, at, na.rm, silent)
      output <- cbind(info[vars], trip)
      return(output)
    })
  output.unlist <- unlist(output)
  output.length <- length(vars) + 4
  output.df <- data.frame(matrix(output.unlist, ncol = output.length, 
                                 byrow = TRUE), 
                          stringsAsFactors = TRUE)
  names(output.df) <- names(output[[1]])
  return(output.df)
}
