% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auc.R
\name{auc}
\alias{auc}
\title{Compute the area under the curve for two vectors}
\usage{
auc(
  x,
  y,
  from = min(x, na.rm = TRUE),
  to = max(x, na.rm = TRUE),
  type = c("linear", "spline"),
  absolutearea = FALSE,
  subdivisions = 100,
  ...
)
}
\arguments{
\item{x}{a numeric vector of x values}

\item{y}{a numeric vector of y values of the same length as x}

\item{from}{The value from where to start calculating the area under the curve. Defaults to the smallest x value.}

\item{to}{The value from where to end the calculation of the area under the curve. Defaults to the greatest x value.}

\item{type}{The type of interpolation. Defaults to "linear" for area under the curve for linear interpolation. The
value "spline" results in the area under the natural cubic spline interpolation.}

\item{absolutearea}{A logical value that determines if negative areas should be added to the total area under the
curve. By default the auc function subtracts areas that have negative y values. Set absolutearea=TRUE to _add_ the
absolute value of the negative areas to the total area.}

\item{subdivisions}{an integer telling how many subdivisions should be used for integrate (for non-linear
approximations)}

\item{...}{additional arguments passed on to approx (for linear approximations). In particular rule can be set to
determine how values outside the range of x is handled.}
}
\value{

}
\description{
Compute the area under the curve using linear or natural spline interpolation for two vectors where one
corresponds to the x values and the other corresponds to the y values. This function wraps the function from the
package "MESS".
}
\examples{
x <- 1:8
set.seed(10); y <- rnorm(8)
auc(x, y)

}
