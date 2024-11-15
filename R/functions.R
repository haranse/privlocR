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

#done to remove some warnings due to set_units using m without quotes
m = 1

default_file_name <- function() {
  return("privlocR_consolidated.gpkg")
}


# create an sf geometry object enveloping all points on the list at a set distance
get_buffered_geometry <- function(lats, longs, dst = units::set_units(1000, m)) {
  return(
    sf::st_buffer(
      sf::st_sfc(
        sf::st_multipoint(
          matrix(c(longs, lats), ncol = 2)
        ),
        crs = 4326
      ),
      dist = dst
    )
  )
}

# get all close features from a single layer
get_single_layer_from_pbf_at_buffered_points <- function(fname, lat, long, layer, dst = units::set_units(1000, m),
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
  gpkg <- osmextract::oe_vectortranslate(fname,
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


  ret <- sf::st_read(gpkg,
    query = query,
    quiet = quiet, wkt_filter = sf::st_as_text(get_buffered_geometry(lat, long, dst = dst))
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
           dst = units::set_units(1000, m),
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
                                              dst = units::set_units(1000, m),
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
  if (!allow_gpkg & any(!is.na(stringr::str_extract(all_fnames, ".gpkg$")))) {
    print("Gpkg files in directory. These might be deleted during processing. Aborting. To override set allow_gpkg = TRUE")
    return(NA)
  }
  pbfs <- all_fnames[!is.na(stringr::str_extract(all_fnames, ".osm.pbf$"))]
  target_fname <- file.path(pbfdir, default_file_name())
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
      sf::st_write(data[[layer]], target_fname, layer = layer, append = TRUE, quiet = quiet)
    }
  }
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
