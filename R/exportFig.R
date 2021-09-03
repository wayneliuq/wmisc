exportFig <- function(figure,
                      filename = "figure",
                      widthpx = 1920,
                      heightpx = 1080,
                      res = 200,
                      format = c("png", "pdf"),
                      dir = "output",
                      prefix = "date",
                      replace = F) {

  ## check and convert output directory

  if (!is.null(dir) & substring(dir, nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }

  ## check valid output format
  if (!any(format %in% c("pdf", "png"))) {
    stop("format must be either 'pdf' and/or 'png'!")
  }

  ## set prefix time with EST time zone

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
    png = fs::path_wd(paste0(dir, prefix_time, filename, ".png")),
    pdf = fs::path_wd(paste0(dir, prefix_time, filename, ".pdf"))
  )

  ## generate unique file name
  ## to do: change this to a function that is agnostic to export file types

  max_suffix <- 100

  if (replace == F) {
    filename_id <- 0
    while (any(fs::file_exists(filenames)) & filename_id < max_suffix) {
      filename_id <- filename_id + 1
      filenames <- c(
        png = fs::path_wd(paste0(dir, prefix_time, filename, "_", sprintf("%02d", filename_id), ".png")),
        pdf = fs::path_wd(paste0(dir, prefix_time, filename, "_", sprintf("%02d", filename_id), ".pdf"))
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

  ## export the figures
  if ("png" %in% format) {
    png(filename = filenames["png"], width = widthpx, height = heightpx, res = res)
    figure
    dev.off()
    message(paste0(filenames["png"], " exported."))
  }

  if ("pdf" %in% format) {
    pdf(file = filenames["pdf"], width= widthpx/144, height = heightpx/144, pointsize = 1)
    print(figure)
    dev.off()
    message(paste0(filenames["pdf"], " exported."))
  }

}
