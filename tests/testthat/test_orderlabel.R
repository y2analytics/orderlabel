#### order_label setup ####
context("order_label")
library(testthat)
library(tibble)
library(dplyr)
library(forcats)
library(y2clerk)
library(orderlabel)

stacked_df <- tibble(
  group_var = c('Brand 1', 'Brand 1', 'Brand 1', 'Brand 2', 'Brand 2', 'Brand 2'),
  value = rep(c('1', '2', '3'), 2),
  label = rep(c('top1', 'top2', 'top3'), 2),
  result = c(.1, .2, .7, .2, .05, .65)
)
stacked_df_top <- tibble(
  group_var = c('Brand 1', 'Brand 1', 'Brand 1', 'Brand 2', 'Brand 2', 'Brand 2'),
  value = rep(c('1', '2', '3'), 2),
  label = rep(c('top1', 'top2', 'top3'), 2),
  result = c(.1, .2, .7, .2, .05, .65),
  topbox = c(rep(.3, 3), rep(.25, 3))
)

ungrouped_df <- tibble(
  variable = rep('X', 5),
  value = c('1', '2', '3', '4', '5'),
  label = c('One', 'Two', 'Three', 'Four', 'Five'),
  result = c(.1, .2, .3, .4, .5),
  n = rep(100, 5)
)
groups <- tibble(
  group_var = c(rep('Brand 1', 5), rep('Brand 2', 5))
)
grouped_df <- bind_rows(ungrouped_df, ungrouped_df) %>%
  bind_cols(groups)

noneother_df <- tibble(
  variable = rep('X', 8),
  value = c('1', '2', '3', '4', '5', '6', '7', '8'),
  label = c('One', 'Two', 'Other', 'None of the above', 'Prefer not to say', 'Three', 'Four', 'Five'),
  result = c(.1, .2, .3, .4, .5, .6, .7, .8),
  n = rep(100, 8)
)
groups <- tibble(
  group_var = c(rep('Brand 1', 8), rep('Brand 2', 8))
)
other_grouped_df <- bind_rows(noneother_df, noneother_df) %>%
  bind_cols(groups)
noneother_grouped2 <- tibble(
  group_var = c(
    'Brand 1', 'Brand 1',
    'Brand 2', 'Brand 2',
    'Other', 'Other',
    'None of the above', 'None of the above',
    'Prefer not to say', 'Prefer not to say',
    'Brand 3', 'Brand 3'
    ),
  variable = rep('X', 12),
  value = rep(c('1', '2'), 6),
  label = rep(c('One', 'Two'), 6),
  result = rep(c(.1, .2), 6),
  n = rep(100, 12)
)

#### Private Functions *************************************** ####
#### PRE- grouped internal functions ####
### blank_values
test_that("blank_values - creates value var", {
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
  # Test that blank_values create values if missing
  expect_equal(test_blank, c('a', 'result', 'n', 'value', 'label'))
  # If not missing, test that it doesn't overwrite the labels/values
  expect_equal(df_normal, test_normal)
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
  df <- tibble(
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
  df <- tibble(
    value = c(3, 2, 1, 4, 5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% factors(grouped = F, group_var = NULL, label_var = label)
  value_values <- unique(df$value)

  expect_equal(value_values, c(1:5))
})


### reverse_label (inherent order)
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
  df <- tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>%
    reverse_label_unordered(rev_label = F)
  values <- purrr::as_vector(df$label)
  df_rev <- tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  ) %>% reverse_label_unordered(rev_label = T)
  values_rev <- purrr::as_vector(df_rev$label)

  expect_equal(values, as.vector(c('One', 'Two', 'Three', 'Four', 'Five')))
  expect_equal(values_rev, rev(as.vector(c('One', 'Two', 'Three', 'Four', 'Five'))))
})


#### (1) Ungrouped Section ####
### Ungrouped1: label_specific
test_that("Ungrouped1: label_specific", {
  df <- tibble(
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
  df <- tibble(
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
  df <- tibble(
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
  df <- tibble(
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
  df <- tibble(
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

#### Grouping functions - not complete ####
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
  df_ungrouped <- tibble(
    label = rep(c('One', 'Two'), 2),
    result = rep(.2, 4),
    n = rep(100, 4),
    group_var = c(1, 1, 2, 2)
  ) %>% factors(grouped = T, group_var = group_var, label_var = label)

  df_grouped <- tibble(
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
  group_forward <- tibble(
    label = c(rep('One', 4), rep('Two', 4)),
    result = rep(.2, 8),
    n = rep(100, 8),
    group_var = rep(c('One', 'Two', 'Three', 'Four'), 2)
  ) %>% factors(grouped = T, group_var = group_var, label_var = label)
  purrr::as_vector(group_forward$group_var)

  group_backward <- tibble(
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

#### topbox ####
# spreading_top2
test_that("spreading_top2 - top1box", {
  test <- spreading_top2(stacked_df, 1)
  created_vector <- test %>% pull(topbox)
  expected_vector <- c(rep(.1, 3), rep(.2, 3))

  expect_equal(created_vector, expected_vector)
})
test_that("spreading_top2 - top2box", {
  test <- spreading_top2(stacked_df, 2)
  created_vector <- test %>% pull(topbox)
  expected_vector <- c(rep(.3, 3), rep(.25, 3))

  expect_equal(created_vector, expected_vector)
})
test_that("spreading_top2 - top3box", {
  test <- spreading_top2(stacked_df, 3)
  created_vector <- test %>% pull(topbox)
  expected_vector <- c(rep(1, 3), rep(.9, 3))

  expect_equal(created_vector, expected_vector)
})
# ordering_top2
test_that("ordering_top2 - top2box", {
  test <- ordering_top2(stacked_df_top)
  created_groups <- test %>% pull(group_var) %>% as.character()
  expected_groups <- c(rep('Brand 1', 3), rep('Brand 2', 3))
  top_percent <- test %>% pull(percent_label)
  created_labels <- test %>% pull(label) %>% as.character()
  expected_labels <- rep(c('top1', 'top2', 'top3'), 2)

  expect_equal(created_groups, expected_groups) # group in order
  expect_equal(top_percent[1], '10%') # top percent should be 10% even though 10<20
  expect_equal(created_labels, expected_labels) # labels should be ordered for stacked
})
# topbox
test_that("topbox - top2box", {
  test <- topbox(stacked_df, 2)
  created_groups <- test %>% pull(group_var) %>% as.character()
  expected_groups <- c(rep('Brand 1', 3), rep('Brand 2', 3))
  top_percent <- test %>% pull(percent_label)
  created_labels <- test %>% pull(label) %>% as.character()
  expected_labels <- rep(c('top1', 'top2', 'top3'), 2)
  if_null <- topbox(stacked_df)

  expect_equal(created_groups, expected_groups) # group in order
  expect_equal(top_percent[1], '10%') # top percent should be 10% even though 10<20
  expect_equal(created_labels, expected_labels) # labels should be ordered for stacked
  expect_equal(if_null, stacked_df)
})

#### label_last & group_last ####
# label_last_fun
test_that("label_last_fun", {
  factored_df <- ungrouped_df %>% mutate(label = fct_inorder(label))
  test <- factored_df %>% label_last_fun(label_last = 'Three')
  last_levels <- levels(test$label)

  expect_equal(last_levels[5], 'Three')
})
# group_last_fun
test_that("group_last_fun", {
  factored_df <- grouped_df %>% mutate(label = fct_inorder(label)) %>%
    mutate(group_var = fct_inorder(group_var))
  test <- factored_df %>% group_last_fun(group_last = 'Brand 1')
  last_levels <- levels(test$group_var)

  expect_equal(last_levels[2], 'Brand 1')
})

# label_last/group_last in order_label
test_that("label_last & group_last", {
  test <- grouped_df %>% order_label(
    group_var = group_var,
    label_last = 'Three',
    group_last = 'Brand 1')
  last_labels <- levels(test$label)
  last_group <- levels(test$group_var)

  expect_equal(last_labels[5], 'Three')
  expect_equal(last_group[2], 'Brand 1')
})

# mixing label_last with other arguments
test_that("label_last & group_last", {
  test <- grouped_df %>% order_label( #these guys are backwards
    group_var = group_var,
    horizontal = T,
    label_last = 'Three')
  last_horizontal <- levels(test$label)

  test <- grouped_df %>% order_label( #these guys are backwards
    group_var = group_var,
    stacked = 'gg',
    label_last = 'Three')
  last_stacked_gg <- levels(test$label)

  test <- grouped_df %>% order_label(
    group_var = group_var,
    stacked = 'ms',
    label_last = 'Three')
  last_stacked_ms <- levels(test$label)

  expect_equal(last_horizontal[5], 'Three')
  expect_equal(last_stacked[5], 'Three')
})


#### horizontal_chart ####
# Not horizontal
test_that("horizontal_chart - not horizontal", {
  test <- horizontal_chart(ungrouped_df, horizontal = F)
  expect_equal(test, ungrouped_df)
})
# Ungrouped
test_that("horizontal_chart - ungrouped", {
  factored_df <- ungrouped_df %>% mutate(label = fct_inorder(label))
  test <- horizontal_chart(factored_df, horizontal = T, grouped = F)
  hor_levels <- levels(test$label)
  expected_levels <- c('Five', 'Four', 'Three', 'Two', 'One')

  expect_equal(hor_levels, expected_levels)
})
# Grouped
test_that("horizontal_chart - grouped", {
  factored_df <- grouped_df %>%
    mutate(label = fct_inorder(label)) %>%
    mutate(group_var = fct_inorder(group_var))

  test <- horizontal_chart(factored_df, horizontal = T, grouped = T)
  hor_levels <- levels(test$label)
  expected_levels <- c('Five', 'Four', 'Three', 'Two', 'One')
  hor_groups <- levels(test$group_var)
  expected_groups <- c('Brand 2', 'Brand 1')

  expect_equal(hor_levels, expected_levels)
  expect_equal(hor_groups, expected_groups)
})

#### stacked_chart + stacked_chart_ms ####
# stacked_chart
test_that("stacked_chart  - ungrouped", {
  factored_df <- ungrouped_df %>% mutate(label = fct_inorder(label))
  test <- stacked_chart(factored_df, stacked = 'gg', grouped = F)
  created_levels <- levels(test$label)
  expected_levels <- c('Five', 'Four', 'Three', 'Two', 'One')

  expect_equal(created_levels, expected_levels)
})
test_that("stacked_chart  - grouped", {
  factored_df <- grouped_df %>% mutate(label = fct_inorder(label)) %>%
    mutate(group_var = fct_inorder(group_var))
  test <- stacked_chart(factored_df, stacked = 'gg', grouped = T)
  created_levels <- levels(test$label)
  expected_levels <- c('Five', 'Four', 'Three', 'Two', 'One')
  created_group <- levels(test$group_var)
  expected_group <- c('Brand 2', 'Brand 1')

  expect_equal(created_levels, expected_levels)
  expect_equal(created_group, expected_group)
})

# stacked_chart
test_that("stacked_chart_ms  - ungrouped", {
  factored_df <- ungrouped_df %>% mutate(label = fct_inorder(label))
  test <- stacked_chart_ms(factored_df, stacked = 'ms', grouped = F)
  created_levels <- levels(test$label)
  expected_levels <- c('One', 'Two', 'Three', 'Four', 'Five')

  expect_equal(created_levels, expected_levels)
})
test_that("stacked_chart_ms  - grouped", {
  factored_df <- grouped_df %>% mutate(label = fct_inorder(label)) %>%
    mutate(group_var = fct_inorder(group_var))
  test <- stacked_chart_ms(factored_df, stacked = 'ms', grouped = T)
  created_levels <- levels(test$label)
  expected_levels <- c('One', 'Two', 'Three', 'Four', 'Five')
  created_group <- levels(test$group_var)
  expected_group <- c('Brand 2', 'Brand 1')

  expect_equal(created_levels, expected_levels)
  expect_equal(created_group, expected_group)
})


#### none_other ####
# Not horizontal
test_that("none_other - none_other = FALSE", {
  test <- none_other(noneother_df, none_other = F, grouped = F)
  expect_equal(test, noneother_df)
})
# Ungrouped
test_that("none_other - ungrouped", {
  factored_df <- noneother_df %>% mutate(label = fct_inorder(label))
  test <- none_other(factored_df, none_other = T, grouped = F)
  created_levels <- levels(test$label)
  expected_levels <- c('One', 'Two', 'Three', 'Four', 'Five',
                       'Other', 'None of the above', 'Prefer not to say')

  expect_equal(created_levels, expected_levels)
})
# Grouped
test_that("none_other - grouped (labels)", {
  factored_df <- other_grouped_df %>% mutate(label = fct_inorder(label)) %>%
    mutate(group_var = fct_inorder(group_var))
  test <- none_other(factored_df, none_other = T, grouped = T)
  created_levels <- levels(test$label)
  expected_levels <- c('One', 'Two', 'Three', 'Four', 'Five',
                       'Other', 'None of the above', 'Prefer not to say')

  expect_equal(created_levels, expected_levels)
})
test_that("none_other - grouped (group_var)", {
  factored_df <- noneother_grouped2 %>% mutate(label = fct_inorder(label)) %>%
    mutate(group_var = fct_inorder(group_var))
  test <- none_other(factored_df, none_other = T, grouped = T)
  created_levels <- levels(test$group_var)
  expected_levels <- c('Brand 1', 'Brand 2', 'Brand 3',
                       'Other', 'None of the above', 'Prefer not to say')

  expect_equal(created_levels, expected_levels)
})

#### num_fmt ####
test_that("num_fmt - error if not percent/general", {
  df_stat <- tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(1, 2, 3, 4, 5),
    n = rep(100, 5)
  )

  expect_error(df_stat %>% order_label(num_fmt = "gnral"))
})
test_that("num_fmt - percents", {
  df_stat <- tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(.1, .2, .3, .4, .5),
    n = rep(100, 5),
    percent_label = c('10', '20', '30', '40', '50%')
  ) %>%
    num_fmt_orderlabel(num_fmt = 'percent')
vectors <- c('10', '20', '30', '40', '50%')

  expect_equal(df_stat$percent_label,vectors)
})
test_that("num_fmt - general", {
  df_stat <- tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(1, 2, 3, 4, 5),
    n = rep(100, 5),
    percent_label = as.character(result)
  ) %>%
    num_fmt_orderlabel(num_fmt = 'general')
vectors <- c('1', '2', '3', '4', '5')

  expect_equal(df_stat$percent_label,vectors)
})



