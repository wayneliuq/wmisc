#' Vectorized calculation of geometric mean
#'
#' @description
#' Geometric mean is a robust measure of central tendency. This function introduces a
#' geometric mean calculation and to calculate the geometric mean for rows and
#' columns of matrix by wrapping functions from the package matrixStats.
#'
#' @param x a numeric vector. Geometric mean can only be calculated for positive numbers.
#' @param na.rm If TRUE, any missing values are ignored, otherwise not.
#' @param lx a numeric matrix.
#'
#' @export
geomean <- function(x, na.rm = T) {
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#' @export
#' @rdname geomean
rowGeomean <- function(lx, na.rm = T) {
  if (any(lx <= 0)) {stop("geometric mean can only be performed on matrices of positive numbers!")}
  exp(matrixStats::rowSums2(log(lx), na.rm = na.rm) / nrow(lx))
}

#' @export
#' @rdname geomean
colGeomean <- function(lx, na.rm = T) {
  if (any(lx <= 0)) {stop("geometric mean can only be performed on matrices of positive numbers!")}
  exp(matrixStats::colSums2(log(lx), na.rm = na.rm) / ncol(lx))
}
