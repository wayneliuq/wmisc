#' Stack a data.frame
#'
#' Stack a data.frame object with a given column index.
#'
#' @param df a data.frame object with at least 3 columns to be stacked.
#' @param index a single integer to specify the column that serves as the index for stacking.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' stackDF(df, index = 1)
#' }
stackDF <- function(df, index = 1) {

  ## check before start
  if (ncol(df) < 3) {
    stop("data.frame to be stacked should have more than 2 columns!")
  }

  if (length(which(1:ncol(df) == index)) != 1) {
    stop("index must be a single integer referencing a column in the df!")
  }

  stackcols <- (1:ncol(df))[which(!(1:ncol(df) %in% index))]

  stack_out <- as.data.frame(matrix(nrow = length(df[,index]) * length(stackcols), ncol = 3))
  colnames(stack_out) <- c("index", "x", "value")

  for (i in seq_along(df[,index])) {
    temp <- data.frame(
      index = df[i, index],
      x = colnames(df)[stackcols],
      value = unlist(df[i, -index])
    )

  id <- c((i * length(stackcols) - (length(stackcols) - 1)):(i * length(stackcols)))

  stack_out[id,] <- temp
  }

  return(stack_out)
}
