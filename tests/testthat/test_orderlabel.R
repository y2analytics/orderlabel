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
test_that("add_group", {
  df_ungrouped <- data.frame(
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5),
    change = c(1, 1, 1, 2, 2)
  ) %>% add_group(grouped = F, group_var = change, label_var = label)
  df_grouped <- data.frame(
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5),
    change = c(1, 1, 1, 2, 2)
  ) %>% add_group(grouped = T, group_var = change, label_var = label)

  test_ungrouped <- df_ungrouped %>% names()
  test_grouped <- df_grouped %>% names()
  expect_equal(test_ungrouped, c('value', 'label', 'result', 'n', 'change'))
  expect_equal(test_grouped, c('value', 'label', 'result', 'n', 'change', 'group_var'))
})
### factors
# value = label
test_that("factors: value = label", {
  df <- data.frame(
    value = c('One', 'Two', 'Three', 'Four', 'Five'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% factors(grouped = F, group_var = NULL, label_var = label)
  value_values <- unique(df$value)

  expect_equal(value_values, c(1:5))
})
# value was non existant
test_that("factors: value was non existant", {
  df <- data.frame(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% factors(grouped = F, group_var = NULL, label_var = label)
  value_values <- unique(df$value)

  expect_equal(value_values, c(1:5))
})
# value = x, likely made that from missing values
test_that("factors: value = x", {
  df <- tibble::tibble(
    value = c(rep('x', 5)),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% factors(grouped = F, group_var = NULL, label_var = label)
  value_values <- unique(df$value)

  expect_equal(value_values, c(1:5))
})
# value = 3, out of order?
test_that("factors: value out of order", {
  df <- tibble::tibble(
    value = c(3, 2, 1, 4, 5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% factors(grouped = F, group_var = NULL, label_var = label)
  value_values <- unique(df$value)

  expect_equal(value_values, c(1:5))
})
### reverse_label
test_that("reverse_label: inherent order", {
  df <- data.frame(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% reverse_label(grouped = F, group_var = NULL, label_var = label, rev_label = F)
  values <- purrr::as_vector(df$value)
  df_rev <- data.frame(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% reverse_label(grouped = F, group_var = NULL, label_var = label, rev_label = T)
  values_rev <- purrr::as_vector(df_rev$value)

  expect_equal(values, c(1:5))
  expect_equal(values_rev, c(5:1))
})
### reverse_label_unordered
test_that("reverse_label_unordered", {
  df <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>%
    reverse_label_unordered(rev_label = F)
  values <- purrr::as_vector(df$label)
  df_rev <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% reverse_label_unordered(rev_label = T)
  values_rev <- purrr::as_vector(df_rev$label)

  expect_equal(values, as.vector(c('One', 'Two', 'Three', 'Four', 'Five')))
  expect_equal(values_rev, rev(as.vector(c('One', 'Two', 'Three', 'Four', 'Five'))))
})


#### Ungrouped Section ####
### Ungrouped1: label_specific
test_that("Ungrouped1: label_specific", {
  df <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(.1, .2, .3, .4, .5),
    n = rep(100, 5)
  ) %>%
    reverse_label( grouped = F, group_var = F, label_var = label, rev_label = F) %>%
    ungrouped1(
      specifically_ordered = T,
      inherent_order_label = F,
      label_specific = 'Three'
    )
  values <- purrr::as_vector(df$label) %>% levels()

  expect_equal(values, as.vector(c('Three', 'Five', 'Four', 'Two', 'One')))
})
### Ungrouped2: label_specific, inherent_order
test_that("Ungrouped2: label_specific, inherent_order", {
  df <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(.1, .2, .3, .4, .5),
    n = rep(100, 5)
  ) %>%
    reverse_label( grouped = F, group_var = F, label_var = label, rev_label = F) %>%
    ungrouped2(
      specifically_ordered = T,
      inherent_order_label = T,
      label_specific = 'Three'
    )
  values <- purrr::as_vector(df$label) %>% levels()

  expect_equal(values, as.vector(c('Three', 'One', 'Two', 'Four', 'Five')))
})
### Ungrouped3: stacked
test_that("Ungrouped3: stacked", {
  df <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(.1, .2, .3, .4, .5),
    n = rep(100, 5)
  ) %>%
    reverse_label( grouped = F, group_var = F, label_var = label, rev_label = F) %>%
    ungrouped3(
      stacked = 'gg'
    )
  values <- purrr::as_vector(df$label) %>% levels()

  expect_equal(values, as.vector(c('Five', 'Four', 'Three', 'Two', 'One')))
})
### Ungrouped4: unordered
test_that("Ungrouped4: unordered", {
  df <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(.1, .2, .3, .4, .5),
    n = rep(100, 5)
  ) %>%
    reverse_label( grouped = F, group_var = F, label_var = label, rev_label = F) %>%
    ungrouped4(
      specifically_ordered = F,
      inherent_order_label = F
    )
  values <- purrr::as_vector(df$label) %>% levels()

  expect_equal(values, as.vector(c('Five', 'Four', 'Three', 'Two', 'One')))
})
### Ungrouped5: unordered
test_that("Ungrouped5: unordered", {
  df <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(.1, .2, .3, .4, .5),
    n = rep(100, 5)
  ) %>%
    reverse_label( grouped = F, group_var = F, label_var = label, rev_label = F) %>%
    ungrouped5(
      specifically_ordered = F,
      inherent_order_label = T
    )
  values <- purrr::as_vector(df$label) %>% levels()

  expect_equal(values, as.vector(c('One', 'Two', 'Three', 'Four', 'Five')))
})

#### Grouping functions ####
### add_group
test_that("add_group", {
  df_ungrouped <- data.frame(
    label = rep(c('One', 'Two'), 2),
    result = rep(.2, 4),
    n = rep(100, 4),
    group_var = c(1, 1, 2, 2)
  ) %>%
    add_group(
      grouped = T,
      group_var = group_var,
      label_var = label
    )
  df_grouped <- data.frame(
    label = rep(c('One', 'Two'), 2),
    result = rep(.2, 4),
    n = rep(100, 4),
    group_var = c(1, 1, 2, 2)
  ) %>%
    dplyr::group_by(group_var) %>%
    add_group(
      grouped = T,
      group_var = group_var,
      label_var = label
  )
  expect_equal(df_ungrouped, df_grouped)
})
### factors
test_that("factors: is grouped", {
  df_ungrouped <- tibble::tibble(
    label = rep(c('One', 'Two'), 2),
    result = rep(.2, 4),
    n = rep(100, 4),
    group_var = c(1, 1, 2, 2)
  ) %>% factors(grouped = T, group_var = group_var, label_var = label)

  df_grouped <- tibble::tibble(
    label = rep(c('One', 'Two'), 2),
    result = rep(.2, 4),
    n = rep(100, 4),
    group_var = c(1, 1, 2, 2)
  ) %>%
    dplyr::group_by(group_var) %>%
    factors(grouped = T, group_var = group_var, label_var = label)
  expect_equal(df_ungrouped, df_grouped)
})
### reverse_group
test_that("reverse_group", {
  group_forward <- tibble::tibble(
    label = c(rep('One', 4), rep('Two', 4)),
    result = rep(.2, 8),
    n = rep(100, 8),
    group_var = rep(c('One', 'Two', 'Three', 'Four'), 2)
  ) %>% factors(grouped = T, group_var = group_var, label_var = label)
  purrr::as_vector(group_forward$group_var)

  group_backward <- tibble::tibble(
    label = c(rep('One', 4), rep('Two', 4)),
    result = rep(.2, 8),
    n = rep(100, 8),
    group_var = rep(c('One', 'Two', 'Three', 'Four'), 2)
  ) %>% factors(grouped = T, group_var = group_var, label_var = label) %>%
    reverse_group(rev_group = T)
  group_levels <- purrr::as_vector(group_backward$group_var)

  expect_equal(group_levels, c('Four', 'Four', 'Three', 'Three', 'Two', 'Two', 'One', 'One'))
})

### reverse_label_unordered2

#### Other special functions ####
