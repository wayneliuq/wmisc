#' Unstacking a data.frame object
#'
#' @description This function reliably unstacks a data frame into a matrix
#' @param df The input data.frame object
#' @param row_id An integer indicating the column id which specifies the rows of the unstacked matrix
#' @param col_id An integer indicating the column id which specifies the columns of the unstacked matrix
#' @param value_id An integer indicating the column id which specifies the values to be filled in the unstacked matrix.
#' @param na_value Specifies whether to replace NA values, such as from missing row/column combinations. Defaults to "NA" (no replacement).
#'
#' @return
#' @export

unstackDF <- function(df, row_id, col_id, value_id, na_value = NA) {

  row_factors <- sort(unique(df[, row_id]))
  col_factors <- sort(unique(df[, col_id]))

  ## check conditions
  if (length(row_factors) > (length(df[, row_id]) / 2)) {
    warning("There are very many unique elements for the defined ROW, are you sure this is correct?")
  }

  if (length(col_factors) > (length(df[, col_id]) / 2)) {
    warning("There are very many unique elements for the defined COLUMN, are you sure this is correct?")
  }

  ## generate and fill values
  value_grid <- expand.grid(row = row_factors, col = col_factors)
  value_grid$match <- paste0(value_grid$row, value_grid$col)
  value_grid$value <-df[match(paste0(value_grid$row, value_grid$col), paste0(df[, row_id], df[, col_id])), value_id]

  ## generate matrix
  unstacked <- matrix(
    data = value_grid$value,
    nrow = length(row_factors),
    ncol = length(col_factors)
  )

  colnames(unstacked) <- col_factors
  rownames(unstacked) <- row_factors

  ## replace NA value

  if (any(is.na(unstacked))) {
    num_na <- length(is.na(unstacked))
    message("Resulting unstacked matrix contains ", num_na, " missing values.")

    if (!is.na(na_value)) {
      unstacked[is.na(unstacked)] <- na_value
    }
  }

  return(unstacked)
}
