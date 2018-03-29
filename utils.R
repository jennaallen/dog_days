prettyDate <- function(d) {
  suppressWarnings(format(as.Date(gsub("T", " ", d), "%Y-%m-%d")))
}

writeBin_filepath <- function(x, path) {
  writeBin(x, path)
  addResourcePath("private", normalizePath(tempdir()))
  return(invisible(path))
}