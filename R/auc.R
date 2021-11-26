#' Compute the area under the curve for two vectors
#'
#' @description Compute the area under the curve using linear or natural spline interpolation for two vectors where one
#' corresponds to the x values and the other corresponds to the y values. This function wraps the function from the
#' package "MESS".
#' @param x a numeric vector of x values
#' @param y a numeric vector of y values of the same length as x
#' @param from The value from where to start calculating the area under the curve. Defaults to the smallest x value.
#' @param to The value from where to end the calculation of the area under the curve. Defaults to the greatest x value.
#' @param type The type of interpolation. Defaults to "linear" for area under the curve for linear interpolation. The
#' value "spline" results in the area under the natural cubic spline interpolation.
#' @param absolutearea A logical value that determines if negative areas should be added to the total area under the
#' curve. By default the auc function subtracts areas that have negative y values. Set absolutearea=TRUE to _add_ the
#' absolute value of the negative areas to the total area.
#' @param subdivisions an integer telling how many subdivisions should be used for integrate (for non-linear
#' approximations)
#' @param ... additional arguments passed on to approx (for linear approximations). In particular rule can be set to
#' determine how values outside the range of x is handled.
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1:8
#' set.seed(10); y <- rnorm(8)
#' auc(x, y)
#'
auc <- function (x,
                 y,
                 from = min(x, na.rm = TRUE),
                 to = max(x, na.rm = TRUE),
                 type = c("linear", "spline"),
                 absolutearea = FALSE,
                 subdivisions = 100, ...)
{
  type <- match.arg(type)
  stopifnot(length(x) == length(y))
  stopifnot(!is.na(from))
  if (length(unique(x)) < 2)
    return(NA)
  if (type == "linear") {
    if (absolutearea == FALSE) {
      values <- approx(x, y, xout = sort(unique(c(from,
                                                  to, x[x > from & x < to]))), ...)
      res <- 0.5 * sum(diff(values$x) * (values$y[-1] +
                                           values$y[-length(values$y)]))
    }
    else {
      o <- order(x)
      ox <- x[o]
      oy <- y[o]
      idx <- which(diff(oy >= 0) != 0)
      newx <- c(x, x[idx] - oy[idx] * (x[idx + 1] - x[idx])/(y[idx +
                                                                 1] - y[idx]))
      newy <- c(y, rep(0, length(idx)))
      values <- approx(newx, newy, xout = sort(unique(c(from,
                                                        to, newx[newx > from & newx < to]))), ...)
      res <- 0.5 * sum(diff(values$x) * (abs(values$y[-1]) +
                                           abs(values$y[-length(values$y)])))
    }
  }
  else {
    if (absolutearea)
      myfunction <- function(z) {
        abs(splinefun(x, y, method = "natural")(z))
      }
    else myfunction <- splinefun(x, y, method = "natural")
    res <- integrate(myfunction, lower = from, upper = to,
                     subdivisions = subdivisions)$value
  }
  res
}
