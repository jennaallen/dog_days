prettyDate <- function(d) {
  suppressWarnings(format(as.Date(gsub("T", " ", d), "%Y-%m-%d")))
}

writeBin_filepath <- function(x, path) {
  writeBin(x, path)
  return(invisible(path))
}