#### order_same ####
context("order_same")


### Ungrouped
test_that("Ungrouped", {
  df <- data.frame(
    label = c(
      'Brand 2',
      'Brand 1',
      'Brand 3',
      'Brand 4',
      'Brand 6',
      'Brand 5'
    ),
    result = c(1:6)
  ) %>%
    dplyr::mutate(
      label = forcats::fct_inorder(label)
    )
  ordered_df <- df
  ordered_levels <- levels(df$label)

  df_unordered <- data.frame(
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4',
      'Brand 5',
      'Brand 6'
    ),
    result = c(1:6)
  ) %>% order_same(orders = ordered_df)
  unordered_levels <- levels(df_unordered$label)

  expect_equal(ordered_levels, unordered_levels)
})

### Grouped
test_that("Grouped", {
  df <- data.frame(
    group_var = c(
      rep('One', 4),
      rep('Two', 4),
      rep('Three', 4)
    ),
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4'
    ),
    result = rep(c(1:4),3)
  ) %>%
    dplyr::mutate(
      label = forcats::fct_inorder(label)
    ) %>%
  dplyr::mutate(
    group_var = forcats::fct_inorder(group_var)
  )
  ordered_df <- df
  lab_lev1 <- levels(df$label)
  group_lev1 <- levels(df$group_var)

  df_unordered <- data.frame(
    group_var = c(
      rep('Two', 4),
      rep('One', 4),
      rep('Three', 4)
    ),
    label = c(
      'Brand 2',
      'Brand 1',
      'Brand 4',
      'Brand 3'
    ),
    result = rep(c(5:2),3)
  ) %>% order_same(orders = ordered_df)
  lab_lev2 <- levels(df$label)
  group_lev2 <- levels(df$group_var)


  expect_equal(lab_lev1, lab_lev2)
  expect_equal(group_lev1, group_lev2)
})

