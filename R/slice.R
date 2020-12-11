# ------------------------------------------------------------------------------
# slice()
#
# Identifies the Location of Each Individual at the Given Time Instant
#
# Description:
# 
# 
# 
# Usage:
#
#   slice(x, at, vars, showProgress = TRUE, na.rm = TRUE, silent = FALSE)
#
# Arguments:
# 
#   x             an object of class ASpaces
#
#   at            a numeric vector of length 1, specifying the time at which 
#                 the location of an individual needs to be extracted
#
#   vars          an optional character vector. See Details.
#
#   showProgress  logical. If TRUE, a progress bar appears on the R console 
#                 while iterating.
#
#   na.rm         logical. If TRUE, trip records with NA values will be removed
#                 and ignored. If FALSE, an error will be generated.
#
#   silent        logical. If TRUE, if any trip records have NAs in the time
#                 fields, a warning message will be provided.
#
#   mc            (temporary arg name). logical. If TRUE, function is operated 
#                 in parallel. See Details.
#
# Details:
#
#   The slice() function attempts to retrieve the locations of people in the 
#   input object 'x' at the given time instant 'at'.
#
#   The function looks into the slot 'trip' in each of the elements in the
#   slot 'data' of the object 'x' (i.e., x@data[[i]]@trip where i = {1 ... n}).
#
#   The 'trip' slot contains a sequence of trips made by each person, and it is
#   assumed that the trips are sorted in ascending order of the departure time
#   (i.e., $o_time). If NOT, this function may not work as intended.
#
#   Parallel processing of function is done through the parallel package. 
# 
# Value:
#
#   A data.frame
#
# Author(s):
#
#   Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
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
    mccl <- makeCluster(detectCores() - 1)
    clusterExport(cl = mccl, ".locate")
  }
  
  if (showProgress & require(pbapply)){
    
    output <- pblapply(df, FUN = function(z, vars) {
      info <- as.data.frame(z@info, stringsAsFactors = FALSE)
      trip <- .locate(z@trip, info$id, at, na.rm, silent)
      output <- cbind(info[vars], trip)
      return(output)
    })
  } else if(mc == TRUE){
    
    output <- parLapply(cl = mccl, X = df, fun = function(z, vars){
      info <- as.data.frame(z@info, stringsAsFactors = FALSE)
      trip <- .locate(z@trip, info$id, at, na.rm, silent)
      output <- cbind(info[vars], trip)
      return(output)
    })
    stopCluster(mccl)
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