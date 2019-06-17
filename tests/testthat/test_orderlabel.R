#### order_label ####
context("order_label")



#### Prep Work ####
### blank_values
test_that("blank_values", {
  df_blank <- data.frame(
    a = c(1:5),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  df_normal <- data.frame(
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )

  test_blank <- blank_values(df_blank) %>% names()
  test_normal <- blank_values(df_normal)
  expect_equal(test_blank, c('a', 'result', 'n', 'value', 'label'))
  expect_equal(levels(df_normal$label), c("Five", "Four", "One", "Three", "Two"))
})
### add_label
test_that("add_label", {
  df_blank <- data.frame(
    a = c(1:5),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  df_normal <- data.frame(
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )

  test_blank <- add_label(df_blank, label) %>% names()
  test_normal <- add_label(df_normal, label) %>% names()
  expect_equal(test_blank, c('a', 'result', 'n', 'value', 'label'))
  expect_equal(test_normal, c('value', 'label', 'result', 'n'))
})
### add_group







