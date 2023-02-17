
test_that("add_ages - gets current year", {
  test <- get_current_year(0)
  is_year <- class(test)
  expect_equal(is_year, 'numeric')
})


test_that("add_ages - year_of_survey", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  test <- get_current_year(year_of_survey = 2000)
  expect_equal(test, 2000)
})


test_that("add_ages - age_var_specified", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  test <- age_var_specified(responses, age_var = age, calculated_year = 2020)
  expect_equal(test$year_born_numeric[1], 2000)
  expect_equal(test$age_numeric[1], 20)
})


test_that("add_ages - year_var_specified", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  test <- year_born_var_specified(responses, year_born_var = year, calculated_year = 2020)
  expect_equal(test$year_born_numeric[1], 2000)
  expect_equal(test$age_numeric[1], 20)
})


test_that("add_ages - runs on year_var", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  test <- add_ages(responses, year_born_var = year)
  expect_error(test, NA)
  expect_equal(test$census_age_groups[3], NA_character_)
  expect_equal(test$census_age_groups[4], '18-34')
})


test_that("add_ages - runs on age_var", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  test <- add_ages(responses, age_var = age)
  expect_error(test, NA)
  expect_equal(test$census_age_groups[3], '18-34')
  expect_equal(test$census_age_groups[4], NA_character_)
})


test_that("add_ages - survey_date_var with year_born_var", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  test <- add_ages(responses, year_born_var = year, survey_date_var = survey_date)
  expect_error(test, NA)
  expect_equal(test$age_numeric, c(21, 20, NA, 21))
})


test_that("add_ages - survey_date_var with age_var", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  test <- add_ages(responses, age_var = age, survey_date_var = survey_date)
  expect_error(test, NA)
  expect_equal(test$year_born_numeric, c(2001, 2002, 2004, NA))
})


test_that("add_ages - use only age_var OR year_var", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  expect_error(
    add_ages(responses, age_var = age, year_born_var = year),
    "You specified both year_born_var and age_var, please specify only one")
})


test_that("add_ages - use only year_of_survey OR survey_date_var", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  expect_error(
    add_ages(responses, age_var = age, year_of_survey = 2022, survey_date_var = survey_date),
    "You specified both year_of_survey and survey_date_var, please specify only one")
})

test_that("add_ages - survey_date_var not a date type", {
  responses <- tibble::tibble(
    year = c(2000, 2001, NA, 2002),
    age = c(20, 19, 18, NA),
    survey_date = c(
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2021, Jan, 1'),
      lubridate::ymd('2022, Jan, 1'),
      lubridate::ymd('2023, Jan, 1')
    ),
    survey_year = c(2021, 2021, 2022, 2023)
  )

  expect_error(
    add_ages(responses, age_var = age, survey_date_var = survey_year),
    "survey_date_var must be a type of date variable, not a numeric")
})
