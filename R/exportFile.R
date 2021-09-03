#' Export data.frame or matrix objects
#'
#' Export a data.frame or matrix into several common formats, and automatically prefixes a
#' date/time field. Also optionally renames exported files to prevent replacing files with
#' the same name.
#'
#' @param object a single 'data.frame' or 'matrix' object that is to be exported.
#'
#' @param filename a character vector of the intended file name of the object to be exported.
#' Defaults to "table".
#' @param prefix a character vector containing one of 'date', 'time', or NULL. 'date'
#' prefixes date (e.g. 2021-09-25), whereas 'time' prefixes both time and date. NULL will
#' remove the prefix altogether.
#' @param file_format a character vector containing one or more of "tsv", "csv", and "xlsx".
#' This species the format(s) of the exported file(s).
#' @param dir a character vector which specifies the location of the exported files relative
#' to the working directory
#' @param replace a logical value specifying whether the function should check for redundant
#' file names. If replace is set to FALSE, a new filename will be iteratively generated with
#' a numeric suffix until a unique file name is reached.
#'
#' @export
#' @examples
#' \dontrun{
#' exportFile(x,
#'            filename = "data.frame",
#'            prefix = "time",
#'            file_format = c("tsv", "xlsx"),
#'            dir = "tables",
#'            replace = T)
#'}

exportFile <- function(
  object,
  filename = "table",
  prefix = "date",
  file_format = c("tsv", "csv", "xlsx"),
  dir = "output/",
  replace = F
) {

  ## check and convert output directory
  ## later update this using package::fs

  if (!is.null(dir) & substring(dir, nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }

  ## check and convert object to be exported to data.frame
  if (any(class(object) %in% c("data.frame", "matrix"))) {
    if (any(class(object) %in% "matrix")) {
      if (is.null(rownames(object))) {
        rownames(object) <- seq(1, nrow(object), 1)
      }
      output <- data.frame(
        rowID = rownames(object),
        object
      )
    } else {output <- object}
  } else {stop("object must be a data.frame or matrix!")}

  ## set prefix time with EST timezone

  if (is.null(prefix)) {
    prefix_time <- NULL
  } else if (any(grep(pattern = paste0("^", prefix), "date"))) {
    prefix_time <- paste0(format(Sys.time(), format = "%F", tz = "EST"), "_")
  } else if (any(grep(pattern = paste0("^", prefix), "time"))) {
    prefix_time <- paste0(format(Sys.time(), format = "%F_%H%M%S", tz = "EST"), "_")
  } else {
    stop("'prefix' must be either set as either 'date', 'time', or NULL!")
  }

  ## create file names
  filenames <- c(
    tsv = fs::path_wd(paste0(dir, prefix_time, filename, ".txt")),
    csv = fs::path_wd(paste0(dir, prefix_time, filename, ".csv")),
    xlsx = fs::path_wd(paste0(dir, prefix_time, filename, ".xlsx"))
  )

  ## generate unique file name
  ## to do: change this to a function that is agnostic to export file types

  max_suffix <- 10000

  if (replace == F) {
    filename_id <- 0
    while (any(fs::file_exists(filenames)) & filename_id < max_suffix) {
      filename_id <- filename_id + 1
      filenames <- c(
        tsv = fs::path_wd(paste0(dir, prefix_time, filename, "_", sprintf("%04d", filename_id), ".txt")),
        csv = fs::path_wd(paste0(dir, prefix_time, filename, "_", sprintf("%04d", filename_id), ".csv")),
        xlsx = fs::path_wd(paste0(dir, prefix_time, filename, "_", sprintf("%04d", filename_id), ".xlsx"))
      )
    }

    if (filename_id >= max_suffix) {
      stop(paste0("Maximum number (", max_suffix - 1, " of file suffixes reached! Consider choosing a new filename!"))
    }
  }

  ## create export directory
  if (!fs::dir_exists(dir)) {
    fs::dir_create(dir)
  }

  if ("tsv" %in% file_format) {
    readr::write_tsv(x = output, file = filenames["tsv"])

    if (file.exists(filenames["tsv"])) {
      message(paste0(filenames["tsv"], " successfully exported."))
    } else {message(paste0("Export error for ", filenames["tsv"]))}
  }
  if ("csv" %in% file_format) {
    readr::write_csv(x = output, file = filenames["csv"])
    if (file.exists(filenames["csv"])) {
      message(paste0(filenames["csv"], " successfully exported."))
    } else {message(paste0("Export error for ", filenames["csv"]))}
  }
  if ("xlsx" %in% file_format) {
    writexl::write_xlsx(x = output, path = filenames["xlsx"])
    if (file.exists(filenames["xlsx"])) {
      message(paste0(filenames["xlsx"], " successfully exported."))
    } else {message(paste0("Export error for ", filenames["xlsx"]))}
  }

}
