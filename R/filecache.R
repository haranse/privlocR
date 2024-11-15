the <- new.env(parent = emptyenv())
the$FILE_CACHE = list()

get_cached_file <- function(pbfdir, dst) {
  if (!cached_file_exists(pbfdir,dst)) {
    the$FILE_CACHE[[pbfdir]] = list(dst = dst, fname = tempfile(fileext = ".gpkg"))
  }
  return(the$FILE_CACHE[[pbfdir]]$fname)
}

clear_cached_file <- function(pbfdir) {
  if (!is.null(the$FILE_CACHE[[pbfdir]])) {
    file.remove(the$FILE_CACHE[[pbfdir]]$fname)
    the$FILE_CACHE[[pbfdir]] = NULL
  }
}

cached_file_exists <- function(pbfdir, dst) {
  if (is.null(the$FILE_CACHE[[pbfdir]])) {
    return(FALSE)
  }
  if (the$FILE_CACHE[[pbfdir]]$dst < dst) {
    return(FALSE)
  }
  return(TRUE)
}
