#### other_rm ####
context("other_rm")



test_that("other_rm", {
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



