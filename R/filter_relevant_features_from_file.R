# filter all features from all layers from all pbf files in a directory that are close to one of the target locations
filter_relevant_features_from_dir <- function(pbfdir, lat, long,
                                              layers = DEFAULT_LAYERS,
                                              dst = units::set_units(1000, m),
                                              tags = DEFAULT_TAGS,
                                              force_refilter = FALSE,
                                              quiet = TRUE) {
  all_fnames <- list.files(pbfdir, full.names = TRUE)
  pbfs <- all_fnames[!is.na(stringr::str_extract(all_fnames, ".osm.pbf$"))]
  target_fname <- get_cached_file(pbfdir, dst, tags, lat, long)

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
