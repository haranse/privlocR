test_that("cache identifies tag change", {
  get_cached_file("hello",units::set_units(10,m),c("a","b","c"),c(1,2,3),c(4,5,6))
  expect_equal(cached_file_exists("hello",units::set_units(10,m),c("b","c"),c(1,2,3),c(4,5,6)),FALSE)
  expect_equal(cached_file_exists("hello",units::set_units(100,m),c("a","b","c"),c(1,2,3),c(4,5,6)),FALSE)
  expect_equal(cached_file_exists("hello",units::set_units(1,m),c("a","b","c"),c(1,2,3),c(4,5,6)),TRUE)
  expect_equal(cached_file_exists("hi",units::set_units(1,m),c("a","b","c"),c(1,2,3),c(4,5,6)),FALSE)
  expect_equal(cached_file_exists("hello",units::set_units(1,m),c("a","b","c"),c(1,2),c(4,5)),FALSE)
})
