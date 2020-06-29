# add_ages set up ---------------------------------------------------------

context("add_ages")
library(testthat)
library(dplyr)
library(orderlabel)

responses <- tibble::tibble(
  year = c(
  2000,
  2001,
  NA,
  2002
  ),
  age = c(
  20,
  19,
  18,
  NA
  )
)


# tests -------------------------------------------------------------------


test_that("add_ages - gets current year", {
  test <- get_current_year()
  is_year <- class(test)
  expect_equal(is_year, 'numeric')
})


test_that("add_ages - age_var_specified", {
  test <- age_var_specified(responses, age_var = age, current_year = 2020)
  expect_equal(test$year_born_numeric[1], 2000)
  expect_equal(test$age_numeric[1], 20)
})


test_that("add_ages - year_var_specified", {
  test <- year_born_var_specified(responses, year_born_var = year, current_year = 2020)
  expect_equal(test$year_born_numeric[1], 2000)
  expect_equal(test$age_numeric[1], 20)
})


test_that("add_ages - runs on year_var", {
  test <- add_ages(responses, year_born_var = year)
  expect_error(test, NA)
  expect_equal(test$census_age_groups[3], NA_character_)
  expect_equal(test$census_age_groups[4], '18-34')
})


test_that("add_ages - runs on age_var", {
  test <- add_ages(responses, age_var = age)
  expect_error(test, NA)
  expect_equal(test$census_age_groups[3], '18-34')
  expect_equal(test$census_age_groups[4], NA_character_)
})

test_that("add_ages - use only age_var OR year_var", {
  expect_error(
    add_ages(responses, age_var = age, year_born_var = year),
    "You specified both year_born_var and age_var, please specify only one")

})

