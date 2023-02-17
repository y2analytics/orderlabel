
test_that("add_regions - upper/lower case", {
  responses <- tibble::tibble(
    state = c(
      'Virginia',
      'UTAH',
      'New York',
      'michigan',
      'Washington DC',
      'Puerto Rico'
    ),
    abbreviations= c(
      'VA',
      'UT',
      'NY',
      'MI',
      'DC',
      NA_character_
    )
  )

  test <- add_regions(responses, state_var = state)

  expect_equal(test$census_region[1], "South")
  expect_equal(test$census_region[2], "West")
  expect_equal(test$census_region[3], "Northeast")
  expect_equal(test$census_region[4], "Midwest")
  expect_equal(test$census_region[5], "South")
  expect_equal(test$census_region[6], NA_character_)
})


test_that("add_regions - abbreviations", {
  responses <- tibble::tibble(
    state = c(
      'Virginia',
      'UTAH',
      'New York',
      'michigan',
      'Washington DC',
      'Puerto Rico'
    ),
    abbreviations= c(
      'VA',
      'UT',
      'NY',
      'MI',
      'DC',
      NA_character_
    )
  )

  test <- add_regions(responses, state_var = abbreviations)

  expect_equal(test$census_region[1], "South")
  expect_equal(test$census_region[2], "West")
  expect_equal(test$census_region[3], "Northeast")
  expect_equal(test$census_region[4], "Midwest")
  expect_equal(test$census_region[5], "South")
})


test_that("add_regions - rename variable", {
  responses <- tibble::tibble(
    state = c(
      'Virginia',
      'UTAH',
      'New York',
      'michigan',
      'Washington DC',
      'Puerto Rico'
    ),
    abbreviations= c(
      'VA',
      'UT',
      'NY',
      'MI',
      'DC',
      NA_character_
    )
  )

  test <- add_regions(responses, state_var = abbreviations, new_name = testy_test)
  expect_equal(names(test)[3], "testy_test")
})

