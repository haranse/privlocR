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

DEFAULT_TAGS = c(
  "landuse", "amenity",
  "natural", "office",
  "shop", "tourism",
  "sport", "leisure",
  "military", "building"
)

DEFAULT_LAYERS = c(
  "lines", "multipolygons",
  "multilinestrings", "points", "other_relations"
)


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
                                                         tags = DEFAULT_TAGS,
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
           layers = DEFAULT_LAYERS,
           dst = units::set_units(1000, m),
           tags = DEFAULT_TAGS,
           quiet = TRUE) {
    ret <- lapply(layers, function(layer) {
      get_single_layer_from_pbf_at_buffered_points(fname, lat, long, layer, dst, tags, quiet = quiet)
    })
    names(ret) <- layers
    return(ret)
  }





feature_set_to_tag_vector <- function(feature_set,
                                      tags = DEFAULT_TAGS) {
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
  if (length(ret) == 0) ret <- c()
  return(ret)
}

feature_list_to_tag_list <- function(feature_list, tags = DEFAULT_TAGS) {
  return(lapply(feature_list, function(x) {
    feature_set_to_tag_vector(x, tags)
  }))
}
