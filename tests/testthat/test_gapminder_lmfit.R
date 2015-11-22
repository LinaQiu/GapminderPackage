context("Model Population VS Year for countries in gapminder dataset.")

test_that("gapminder_lmfit() returns a list",{
  library(gapminder)
  test_country <- gapminder_lmfit("Canada")
  expect_is(test_country,"list")
})

test_that("Country not found results in error",{
  library(gapminder)
  test_country <- gapminder_lmfit("Lol","year","pop",offset = 1952)
  expect_identical(test_country,"The country name you provided not found in gapminder. Please try an existing country name.")
})

test_that("Empty country name provide results in error",{
  test_country <- gapminder_lmfit("")
  expect_identical(test_country,"Please provide a country name that you would like to explore.")
})

test_that("No parameter passed",{
  expect_error( gapminder_lmfit())
})

test_that("Whether parameter y has a valid value",{
  library(gapminder)
  test_country <- gapminder_lmfit("Canada","year","continent")
  expect_identical(test_country,"Invalid parameter y. You can leave y equal to its default value 'pop' or set it as pop/lifeExp/gdpPercap.")
})
