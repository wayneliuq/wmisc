#' Centering a numeric matrix
#'
#' @description
#' Plotting heatmaps of gene expression data in numeric matrices often is the most clear when
#' each gene is centered on 0. Since most expression data is log2 transformed, scaling of data
#' is not desirable.
#'
#' @param lx a numeric matrix
#' @param MARGIN a vector giving the subscripts which the function will be applied over. E.g.,
#' for a matrix 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns.
#' @param scale either a logical value or a numeric-alike vector of length equal to the number
#' of columns of x.
#'
#' @return
#' @export
matrixCenter <- function(lx, MARGIN = 1, scale = F) {

  if (!"matrix" %in% class(lx)) {stop("lx must be a numeric matrix!")}

  center <- lx

  if (1 %in% MARGIN) {
    center <- t(scale(t(lx), center = T, scale = scale))
    rownames(center) <- rownames(lx)
    colnames(center) <- colnames(lx)
  }

  if (2 %in% MARGIN) {
    center <- scale(center, center = T, scale = scale)
    rownames(center) <- rownames(lx)
    colnames(center) <- colnames(lx)
  }

  return(center)
}
