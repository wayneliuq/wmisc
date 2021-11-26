#' Calculate confidence interval from standard deviation
#'
#' @param mean the mean of the sample
#' @param sd the standard deviation of the sample
#' @param n the number of samples
#' @param alpha the % confidence interval to be calculated, defaults to 0.95
#'
#' @return returns the lower and upper boundaries of the confidence interval
#' @export
CIfromSD <- function(mean, sd, n, alpha = 0.95) {
  chip <- 1- ((1 - alpha) / 2)
  error <- qt(chip, df = n - 1) * sd / sqrt(n)
  ci = c(mean - error, mean + error)
  return(ci)
}
