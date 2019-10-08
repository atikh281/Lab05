test_that("Check that column 1 is Numeric", {
  expect_equal(is.numeric(regions()$regionid), TRUE)
})
test_that("Check that column 2 is character", {
  expect_equal(is.character(regions()$shortname), TRUE)
})

test_that("Check that sum of perc is 100", {
  expect_equal(sum(check_fuel_region_time()$perc), 100)
})

test_that("Check the different kinds of fuel", {
  expect_equal(check_fuel_region_time()$fuel,c("biomass","coal","imports","gas","nuclear","other","hydro","solar","wind"))
})
