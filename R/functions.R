# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


get_close_elements <- function(long, lat, elements, dst = set_units(100, m)) {
  mypoint <- st_sfc(st_point(c(long, lat)))
  st_crs(mypoint) <- st_crs(elements)
  sf_use_s2(FALSE)
  elements$distance <- st_distance(elements, mypoint)
  sf_use_s2(TRUE)
  return(elements[elements$distance < dst, ])
}

get_close_tags <- function(lat, long, elements, tags = DEFAULT_TAGS, dst = set_units(100, m)) {
  locations <- get_close_elements(long, lat, elements)
  ret <- c()
  for (t in tags) {
    vals <- unique(locations[[t]])
    vals <- ifelse(vals == "yes", t, vals)
    ret <- unique(c(ret, vals))
  }
  return(ret)
}

# create an sf geometry object enveloping all points on the list at a set distance
get_buffered_geometry <- function(lats, longs, dst = set_units(1000, m)) {
  return(
    st_buffer(
      st_sfc(
        st_multipoint(
          matrix(c(longs, lats), ncol = 2)
        ),
        crs = 4326
      ),
      dist = dst
    )
  )
}

# get all close features from a single layer
get_single_layer_from_pbf_at_buffered_points <- function(fname, lat, long, layer, dst = set_units(1000, m),
                                                         tags = c(
                                                           "landuse", "amenity",
                                                           "natural", "office",
                                                           "shop", "tourism",
                                                           "sport", "leisure",
                                                           "military", "building"
                                                         ),
                                                         keep_file = FALSE,
                                                         quiet = TRUE) {
  # Get all data from the relevant area
  gpkg <- oe_vectortranslate(fname,
    layer = layer,
    boundary = get_buffered_geometry(lat, long, dst = dst),
    extra_tags = tags,
    quiet = quiet
  )
  # Query to get only features for which one of the tags is not NA
  query <- paste0(
    "SELECT * FROM ",
    layer,
    " WHERE (",
    paste0(tags, collapse = " IS NOT NULL) OR ("),
    " IS NOT NULL)"
  )


  ret <- st_read(gpkg,
    query = query,
    quiet = quiet, wkt_filter = st_as_text(get_buffered_geometry(lat, long, dst = dst))
  )
  file.remove(gpkg)
  return(ret)
}

# get all close features from all layers from a single pbf
get_all_layers_from_pbf_at_buffered_points <-
  function(fname, lat, long,
           layers = c(
             "lines", "multipolygons",
             "multilinestrings", "points", "other_relations"
           ),
           dst = set_units(1000, m),
           tags = c(
             "landuse", "amenity",
             "natural", "office",
             "shop", "tourism",
             "sport", "leisure",
             "military", "building"
           ),
           quiet = TRUE) {
    ret <- lapply(layers, function(layer) {
      get_single_layer_from_pbf_at_buffered_points(fname, lat, long, layer, dst, tags, quiet = quiet)
    })
    names(ret) <- layers
    return(ret)
  }


# filter all features from all layers from all pbf files in a directory that are close to one of the target locations
filter_relevant_features_from_dir <- function(pbfdir, lat, long,
                                              layers = c(
                                                "lines", "multipolygons",
                                                "multilinestrings", "points", "other_relations"
                                              ),
                                              dst = set_units(1000, m),
                                              tags = c(
                                                "landuse", "amenity",
                                                "natural", "office",
                                                "shop", "tourism",
                                                "sport", "leisure",
                                                "military", "building"
                                              ),
                                              quiet = TRUE,
                                              allow_gpkg = FALSE) {
  all_fnames <- list.files(pbfdir, full.names = TRUE)
  if (!allow_gpkg & any(!is.na(str_extract(all_fnames, ".gpkg$")))) {
    print("Gpkg files in directory. These might be deleted during processing. Aborting. To override set allow_gpkg = TRUE")
    return(NA)
  }
  pbfs <- all_fnames[!is.na(str_extract(all_fnames, ".osm.pbf$"))]
  target_fname <- file.path(pbfdir, "privlocR_consolidated.gpkg")
  if (file.exists(target_fname)) file.remove(target_fname)

  for (fname in pbfs) {
    if (!quiet) {
      print(paste0("Processing ", fname))
    }
    data <- get_all_layers_from_pbf_at_buffered_points(
      fname, lat, long,
      layers, dst, tags,
      quiet
    )
    for (layer in layers) {
      st_write(data[[layer]], target_fname, layer = layer, append = TRUE, quiet = quiet)
    }
  }
}

get_features_close_to_point <- function(pbfdir, lat, long,
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
                                        dst = set_units(100, m),
                                        refilter_data = FALSE,
                                        allow_gpkg = FALSE,
                                        quiet = TRUE) {
  target_fname <- file.path(pbfdir, "privlocR_consolidated.gpkg")
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
        return(st_read(target_fname, layer,
          wkt_filter =
            st_as_text(get_buffered_geometry(lat[idx], long[idx], dst = dst)),
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

feature_set_to_tag_vector <- function(feature_set,
                                      tags = c(
                                        "landuse", "amenity",
                                        "natural", "office",
                                        "shop", "tourism",
                                        "sport", "leisure",
                                        "military", "building"
                                      )) {
  ret <- c()
  for (t in tags) {
    vec <- unlist(sapply(feature_set, function(x) {
      x[[t]]
    }))
    vec <- unique(vec[!is.na(vec)])
    if (length(vec) > 0) {
      ret <- c(ret, paste0(t, "_", vec))
    }
  }
  if (length(ret) == 0) ret <- "none"
  return(ret)
}

feature_list_to_tag_list <- function(feature_list, tags = c(
                                       "landuse", "amenity",
                                       "natural", "office",
                                       "shop", "tourism",
                                       "sport", "leisure",
                                       "military", "building"
                                     )) {
  return(lapply(feature_list, function(x) {
    feature_set_to_tag_vector(x, tags)
  }))
}
