# General -----------------------------------------------------------------

test_that("other_rm - label", {
  df <- data.frame(
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4',
      'Brand 5',
      'Other, please specify',
      'Prefer not to answer...',
      'I find none of these brands appealing',
      'Brand 6',
      'Brand 7 (some description)'
    ),
    result = c(1:10)
  ) %>%
    dplyr::mutate(
      group_var = label,
      variable = label
    )

  test <- other_rm(df)
  expect_equal(test$label[6], 'Other')
  expect_equal(test$label[7], 'Prefer not to say')
  expect_equal(test$label[8], 'None of the above')
  expect_equal(test$label[10], 'Brand 7')
})


test_that("other_rm - variable", {
  df <- data.frame(
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4',
      'Brand 5',
      'Other, please specify',
      'Prefer not to answer...',
      'I find none of these brands appealing',
      'Brand 6',
      'Brand 7 (some description)'
    ),
    result = c(1:10)
  ) %>%
    dplyr::mutate(
      group_var = label,
      variable = label
    )

  test <- other_rm(df)
  expect_equal(test$variable[6], 'Other')
  expect_equal(test$variable[7], 'Prefer not to say')
  expect_equal(test$variable[8], 'None of the above')
  expect_equal(test$variable[10], 'Brand 7')
})


test_that("other_rm - group_var", {
  df <- data.frame(
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4',
      'Brand 5',
      'Other, please specify',
      'Prefer not to answer...',
      'I find none of these brands appealing',
      'Brand 6',
      'Brand 7 (some description)'
    ),
    result = c(1:10)
  ) %>%
    dplyr::mutate(
      group_var = label,
      variable = label
    )

  test <- other_rm(df)
  expect_equal(test$group_var[6], 'Other')
  expect_equal(test$group_var[7], 'Prefer not to say')
  expect_equal(test$group_var[8], 'None of the above')
  expect_equal(test$group_var[10], 'Brand 7')
})


test_that("other_rm - no variable, no group_var", {
  df <- data.frame(
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4',
      'Brand 5',
      'Other, please specify',
      'Prefer not to answer...',
      'I find none of these brands appealing',
      'Brand 6',
      'Brand 7 (some description)'
    ),
    result = c(1:10)
  )

  test <- other_rm(df)
  expect_equal(test$label[6], 'Other')
  expect_equal(test$label[7], 'Prefer not to say')
  expect_equal(test$label[8], 'None of the above')
  expect_equal(test$label[10], 'Brand 7')
})



# Arguments ---------------------------------------------------------------

test_that("other_rm - remove argument", {
  df <- data.frame(
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4',
      'Brand 5',
      'Other, please specify',
      'Prefer not to answer...',
      'I find none of these brands appealing',
      'Brand 6',
      'Brand 7 (some description)'
    ),
    result = c(1:10)
  ) %>%
    dplyr::mutate(
      group_var = label,
      variable = label
    )
  filtered_df <- df %>%
    dplyr::filter(stringr::str_detect(label, 'Brand')) %>%
    dplyr::mutate_all(~ifelse(stringr::str_detect(., '7'), 'Brand 7', .))

  test <- other_rm(df, remove = TRUE)
  expect_equal(test, filtered_df)
})


test_that("other_rm - var argument", {
  df <- data.frame(
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4',
      'Brand 5',
      'Other, please specify',
      'Prefer not to answer...',
      'None',
      'Brand 6',
      'Brand 7 (some description)'
    ),
    result = c(1:10)
  ) %>%
    dplyr::mutate(
      other_var = label
    )

  test <- df %>% other_rm(var = other_var)
  expect_equal(test$other_var[6], 'Other')
  expect_equal(test$other_var[7], 'Prefer not to say')
  expect_equal(test$other_var[8], 'None of the above')
  expect_equal(test$other_var[10], 'Brand 7')
})


