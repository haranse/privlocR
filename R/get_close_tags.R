#done to remove some warnings due to set_units using m without quotes
m = 1


#' Get tags close to a series of locations
#'
#' `get_close_tags()` returns relevant tags which are at a certain distance from each location, giving a
#' meaningful sense of the type of place the person is in without providing exact data. No data is sent
#' online.
#' @param pbfdir File directory with .osm.pbf files containing map data for the relevant area. During processing the function will create a .p
#' @param lat numeric vector of latitudes
#' @param long numeric vector of longitudes
#' @param layers relevant layers from the pbf files
#' @param tags location tags of interest
#' @param dst
#' @param refilter_data
#' @param allow_gpkg
#' @param quiet
#'
#' @return
#' @export
#'
#' @examples
get_close_tags <- function(pbfdir, lat, long,
                               layers = DEFAULT_LAYERS,
                               tags = DEFAULT_TAGS,
                               dst = units::set_units(100, m),
                               refilter_data = FALSE,
                               quiet = TRUE) {
  return(feature_list_to_tag_list(get_close_features(pbfdir, lat, long, layers, tags, dst, refilter_data, quiet)))
}


get_close_features <- function(pbfdir, lat, long,
                               layers = DEFAULT_LAYERS,
                               tags = DEFAULT_TAGS,
                               dst = units::set_units(100, m),
                               refilter_data = FALSE,
                               quiet = TRUE) {
  if (cached_file_exists(pbfdir,dst) & !refilter_data) {
    if (!quiet) print("Existing filtered file detected, continuing with analysis (to refilter use refilter_data = TRUE")
  } else {
    clear_cached_file(pbfdir)
    filter_relevant_features_from_dir(pbfdir, lat, long, layers, dst, tags, refilter_data, quiet)
  }
  nvals <- length(lat)
  if (length(long) != nvals) {
    print("Different numbers of latitude and longitude values, aborting")
    return(NA)
  }

  repeats <- c(nvals, sapply(2:nvals, function(x) {
    min(c(which((lat[1:(x - 1)] == lat[x]) & (long[1:(x - 1)] == long[x])), nvals))
  }))
  if (!quiet) {
    print(paste0("Skipping ", sum(repeats < nvals), " duplicates out of ", nvals))
  }

  target_fname <- get_cached_file(pbfdir,dst)

  ret <- lapply(1:nvals, function(idx) {
    if ((repeats[idx]) == nvals) {
      subret <- lapply(layers, function(layer) {
        return(sf::st_read(target_fname, layer,
          wkt_filter =
            sf::st_as_text(get_buffered_geometry(lat[idx], long[idx], dst = dst)),
          quiet = TRUE
        ))
      })
      names(subret) <- layers
    } else {
      subret <- NA
    }
    return(subret)
  })
  for (i in 1:nvals) {
    if (repeats[i] < nvals) ret[[i]] <- ret[[repeats[i]]]
  }
  return(ret)
}
