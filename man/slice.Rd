\name{slice}
\alias{slice}
\title{Population Distribution at a Specific Point in Time}
\description{
From a given data set of class \code{ASpaces}, the function finds the location 
of each individual at a specific point in time and returns the results as a
\code{data.frame} object.
}
\usage{
slice(x, at, vars)
}
\arguments{
  \item{x}{an object of class \code{ASpaces}.}
  \item{at}{a numeric vector of length 1, or an object of class \code{POSIXct}.}
  \item{vars}{a character vector to specify the variables .}
}

\details{
The purpose of \code{slice} is this.
}

\value{
An object of \code{data.frame}.
}
\note{
An extra details come here.
}
\author{
Seong-Yun Hong
}
\seealso{
\code{\link{slice_to_df}}, \code{\link{ASpaces-class}}
}

\examples{
# examples
x <- 1:10
slice(x)

\dontrun{
# examples that do not run
y <- runif(10)
slice(y)
}
}