#done to remove some warnings due to set_units using m without quotes
m = 1


get_close_tags <- function(pbfdir, lat, long,
                               layers = c(
                                 "lines", "multipolygons",
                                 "multilinestrings", "points",
                                 "other_relations"
                               ),
                               tags = c(
                                 "landuse", "amenity",
                                 "natural", "office",
                                 "shop", "tourism",
                                 "sport", "leisure",
                                 "military", "building"
                               ),
                               dst = units::set_units(100, m),
                               refilter_data = FALSE,
                               allow_gpkg = FALSE,
                               quiet = TRUE) {
  return(feature_list_to_tag_list(get_close_features(pbfdir, lat, long, layers, tags, dst, refilter_data, allow_gpkg, quiet)))
}


get_close_features <- function(pbfdir, lat, long,
                               layers = c(
                                 "lines", "multipolygons",
                                 "multilinestrings", "points",
                                 "other_relations"
                               ),
                               tags = c(
                                 "landuse", "amenity",
                                 "natural", "office",
                                 "shop", "tourism",
                                 "sport", "leisure",
                                 "military", "building"
                               ),
                               dst = units::set_units(100, m),
                               refilter_data = FALSE,
                               allow_gpkg = FALSE,
                               quiet = TRUE) {
  target_fname <- file.path(pbfdir, default_file_name())
  if (file.exists(target_fname) & !refilter_data) {
    if (!quiet) print("Existing filtered file detected, continuing with analysis (to refilter use refilter_data = TRUE")
  } else {
    if (file.exists(target_fname) & refilter_data) file.remove(target_fname)
    filter_relevant_features_from_dir(pbfdir, lat, long, layers, dst, tags, quiet, allow_gpkg)
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
