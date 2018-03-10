prettyDate <- function(d) {
  suppressWarnings(format(as.Date(gsub("T", " ", d), "%Y-%m-%d")))
}