the <- new.env(parent = emptyenv())
the$FILE_CACHE = list()

get_cached_file <- function(pbfdir, dst, tags, lat, long) {
  if (!cached_file_exists(pbfdir,dst, tags, lat, long)) {
    clear_cached_file(pbfdir)
    the$FILE_CACHE[[pbfdir]] = list(dst = dst, fname = tempfile(fileext = ".gpkg"), tags = tags, lat = lat, long = long)
  }
  return(the$FILE_CACHE[[pbfdir]]$fname)
}

clear_cached_file <- function(pbfdir) {
  if (!is.null(the$FILE_CACHE[[pbfdir]])) {
    file.remove(the$FILE_CACHE[[pbfdir]]$fname)
    the$FILE_CACHE[[pbfdir]] = NULL
  }
}

cached_file_exists <- function(pbfdir, dst, tags, lat, long) {
  if (is.null(the$FILE_CACHE[[pbfdir]])) {
    return(FALSE)
  }
  if (the$FILE_CACHE[[pbfdir]]$dst < dst) {
    return(FALSE)
  }
  if (!identical(the$FILE_CACHE[[pbfdir]]$tags,tags)) {
    return(FALSE)
  }
  if (!identical(the$FILE_CACHE[[pbfdir]]$lat, lat) | !identical(the$FILE_CACHE[[pbfdir]]$long, long)) {
    return(FALSE)
  }
  return(TRUE)
}
