#' Subsets the Input object of Class ASpaces
#' @export
#' @description Extract \code{ASpace} objects with characteristics that meet given conditions from \code{ASpaces}
#' @usage subset(x, condition, vars, all = TRUE, showProgress = TRUE)
#'        
#' @param x an object of class \code{ASpaces}
#' @param condition a list that indicate characteristics. 
#' @param vars an optional character vector. See details.
#' @param all logical. See details.
#' @param showProgress logical. If TRUE, a progress bar appears on the R console 
#'                 while iterating.
#' @return An object of class ASpaces
#' @details \code{subset} function is a function that refines a given \code{ASpaces} 
#' class object.
#' 
#' \code{ASpace} classes contain the characteristics 
#' of the activity space subject in the info slot, and \code{subset} function 
#' extracts only \code{ASpaces} that meet the given conditions based on the 
#' characteristics of#' this info slot and returns them to \code{ASpaces}.
#' 
#' 'condition' is entered in the form of a \code{list}, and the \code{ASpace} 
#' of the corresponding condition is extracted by entering each desired 
#' condition in the \code{list} (i.e., \code{list(var1 = x, var2 = y)})
#' 
#' 'vars' are used to refine the information in the resulting \code{ASpace}'s 
#' \code{info} slot. When a specific character vector is input to 'vars', the 
#' \code{info} slot of \code{ASpace} resulting from the function has only the 
#' input variables. If 'vars' is not input, the result is same with the original 
#' \code{info} slot.
#' 
#' 'all' determines whether to extract only the results that satisfy all of them 
#' when various conditions are given in the 'condition', or to extract the 
#' results that apply at least one. The default value is set to extract only 
#' when all conditions are satisfied as true.
#' @author Changlock Choi (hihi7100@khu.ac.kr), Seong-Yun Hong (syhong@khu.ac.kr)
#' @examples
#' 
#' # load data
#' data(slicedata)
#' 
#' # subsetting ASpaces
#' subsetasp1 <- subset(slicedata, condition = list(sex = 1))
#' subsetasp2 <- subset(slicedata, condition = list(sex = 1, hhsize = 4))
#' subsetasp3 <- subset(slicedata, condition = list(sex = 1, hhsize = 4), 
#'                      all = FALSE)
#'                      
subset <- function(x, condition, vars, all = TRUE, showProgress = TRUE) {
  
  # ----------------------------------------------------------------------------
  # Is 'x' a valid ASpaces object?
  # ----------------------------------------------------------------------------
  if (!inherits(x, "ASpaces"))
    stop("'x' must be of class ASpaces", call. = FALSE)
  else
    data.list <- slot(x, "data")
  
  # ----------------------------------------------------------------------------
  # If 'condition' is given:
  # ----------------------------------------------------------------------------
  if (!missing(condition)) {
    if (showProgress & require(pbapply)) {
      cat("Finding elements satisfying the given conditions\n")
      INDEX <- pbsapply(data.list, function(z) .select(z, condition, all))
    } else {
      INDEX <- sapply(data.list, function(z) .select(z, condition, all))
    }
      
    data.list <- data.list[INDEX]
  }
  
  # ----------------------------------------------------------------------------
  # If 'vars' is given:
  # ----------------------------------------------------------------------------
  if (!missing(vars)) {
    if (showProgress & require(pbapply)) {
      cat("Dropping the variables not included in 'vars'\n")
      data.list <- pblapply(data.list, function(z) {
        z@info <- z@info[vars]
        return(z)
      })
    } else {
      data.list <- lapply(data.list, function(z) {
        z@info <- z@info[vars]
        return(z)
      })
    }
  }
  
  output <- update(x, data = data.list)
  return(output)
}