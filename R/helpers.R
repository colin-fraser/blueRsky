datetime_format <- function(x) {
  format(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

view_schema <- function(s) {
  s <- load_schema(s)
  cat(jsonlite::toJSON(s, pretty = TRUE, auto_unbox = TRUE))
}

