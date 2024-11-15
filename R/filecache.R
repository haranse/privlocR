the <- new.env(parent = emptyenv())
the$FILE_CACHE = list()

get_cached_file <- function(pbfdir, dst, tags) {
  if (!cached_file_exists(pbfdir,dst, tags)) {
    clear_cached_file(pbfdir)
    the$FILE_CACHE[[pbfdir]] = list(dst = dst, fname = tempfile(fileext = ".gpkg"), tags = tags)
  }
  return(the$FILE_CACHE[[pbfdir]]$fname)
}

clear_cached_file <- function(pbfdir) {
  if (!is.null(the$FILE_CACHE[[pbfdir]])) {
    file.remove(the$FILE_CACHE[[pbfdir]]$fname)
    the$FILE_CACHE[[pbfdir]] = NULL
  }
}

cached_file_exists <- function(pbfdir, dst, tags) {
  if (is.null(the$FILE_CACHE[[pbfdir]])) {
    return(FALSE)
  }
  if (the$FILE_CACHE[[pbfdir]]$dst < dst) {
    return(FALSE)
  }
  if (!identical(the$FILE_CACHE[[pbfdir]]$tags,tags)) {
    return(FALSE)
  }
  return(TRUE)
}
