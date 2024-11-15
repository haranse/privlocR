test_that("get_close_tags returns correct tags", {
  #create temporary directory and move sample tokelau data into it
  mydir <- withr::local_tempdir()
  file.copy(system.file("extdata", "tokelau.osm.pbf", package = "privlocR"), mydir)



  #test sample locations
  mytags = (get_close_tags(mydir, c(-9.1979860, -9.192079), c(-171.8501176, -171.856883)))
  expect_equal(mytags[[1]],c("landuse_residential","amenity_restaurant","natural_reef","natural_coastline",
                             "natural_scrub","tourism_hotel","leisure_park","building_yes"))
  expect_equal(mytags[[2]],c("natural_reef","natural_coastline","natural_water","natural_wood","building_yes"))

  #test sample locations with different distance
  mytags = (get_close_tags(mydir, c(-9.1979860, -9.192079), c(-171.8501176, -171.856883), dst = units::set_units(10,m)))
  expect_equal(mytags[[1]],c("landuse_residential","amenity_restaurant","natural_reef","natural_coastline",
                             "tourism_hotel","building_yes"))
  expect_equal(mytags[[2]],c("natural_reef","natural_coastline"))

})
