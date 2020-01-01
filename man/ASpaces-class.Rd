\name{ASpaces-class}
\docType{class}
\alias{extract}

\title{Class ASpaces}
\description{A S4 class to hold activity space data.}
\section{Objects from the Class}{
Objects can be created by calls to the constructor \code{\link{ASpaces}}.
}
\section{Slots}{
  \describe{
    \item{data}{an object of class \code{list} containing the spatial dissimilarity index value.}
    \item{attr}{an object of class \code{list} containing the spatial diversity index value.}
    \item{sp}{an object of class \code{Spatial} containing the spatial information theory index value.}
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "SegSpatial", to = "SpatialPoints")}: coerce an object of class \code{SegSpatial} to an object of class \code{SpatialPoints}. The points have no attribute data.}
    \item{coerce}{\code{signature(from = "SegSpatial", to = "SpatialPointsDataFrame")}: coerce an object of class \code{SegSpatial} to an object of class \code{SpatialPointsDataFrame}. The values in the slot \sQuote{data} will be used as the attribute data.}
    \item{coerce}{\code{signature(from = "SegSpatial", to = "SpatialPixelsDataFrame")}: coerce an object of class \code{SegSpatial} to an object of class \code{SpatialPixelsDataFrame}. The values in the slot \sQuote{data} will be used as the attribute data. May not work when the points are irregularly spaced.}
    \item{coerce}{\code{signature(from = "SegSpatial", to = "list")}: retrieve the segregation index values and return it as a \code{list} object.}
    \item{as.list}{\code{signature(x = "SegSpatial")}: same as the above.}
    \item{show}{\code{signature(object = "SegSpatial")}: show the segregation index values.}
    \item{print}{\code{signature(x = "SegSpatial")}: same as \code{show}.}
    \item{spplot}{\code{signature(obj = "SegSpatial")}: coerce an object of class \code{SegSpatial} to an object of class \code{SpatialPixelsDataFrame} or \code{SpatialPointsDataFrame} and display it. See \code{help(spplot)} for more details about the graphical parameter arguments.}
  }
}

\section{Extends}{
\code{\link{ASpace-class}}.
}

\author{Seong-Yun Hong}
\seealso{
\code{\link{ASpace}}, \code{\link{slice}}, \code{\link{extract}}
}
\examples{
# creates 100 regularly-spaced data points and 3 population groups
xy <- expand.grid(1:10, 1:10)
colnames(xy) <- c("x", "y")
pop <- matrix(runif(300), ncol = 3)
env <- matrix(runif(300), ncol = 3)
colnames(pop) <- LETTERS[1:3]
colnames(env) <- LETTERS[4:6]

# constructs an object of class 'SegSpatial'
v <- SegSpatial(d = numeric(), r = numeric(), h = numeric(), 
                p = matrix(0, 0, 0), 
                coords = as.matrix(xy), data = pop, env = env)
print(v)

# changes the spatial dissimilarity index value
slot(v, "d") <- runif(1)

# retrieves the index values
as.list(v)

# displays the values in the slot 'data'
spplot(v, col.regions = heat.colors(20))

# displays the values in the slot 'env'
w <- as(v, "SegLocal")
spplot(w, col.regions = heat.colors(20))
}
\keyword{classes}