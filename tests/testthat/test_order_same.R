
# Errors ------------------------------------------------------------------

test_that("Error - label not factored", {
  ordered_df <- data.frame(
    label = c(
      'Brand 1',
      'Brand 2',
      'Brand 3',
      'Brand 4'
    ),
    result = rep(c(1:4),3)
  )

  expect_error(
    ordered_df %>% order_same(ordered_df),
    'The "label" variable in your "orders" data frame is not factored in a specific order. Please order your "orders" data frame before proceeding.'
  )
})


test_that("Error - group_var not factored", {
  frequencies <- data.frame(
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
    )

  expect_error(
    frequencies %>% order_same(frequencies),
    'The "group_var" variable in your "orders" data frame is not factored in a specific order. Please order your "orders" data frame before proceeding.'
  )
})


test_that("Error - group_var in orders", {
  frequencies <- data.frame(
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
  )
  ordered_df <-  frequencies %>%
    dplyr::mutate(
      label = forcats::fct_inorder(label)
    ) %>%
    dplyr::select(-group_var)

  expect_error(
    frequencies %>% order_same(ordered_df, group_var = group_var),
    'If using the group_var argument, the data frame from the "orders" argument must have a column named "group_var". This will be the column by which your new data frame is ordered.'
  )
})



# Overall -----------------------------------------------------------------

test_that("Ungrouped frequencies", {
  frequencies <- data.frame(
    label = c(
      'Brand 2',
      'Brand 1',
      'Brand 3',
      'Brand 4',
      'Brand 6',
      'Brand 5'
    ),
    result = c(1:6)
  )
  ordered_df <- frequencies %>%
    dplyr::mutate(
      label = forcats::fct_inorder(label)
    )
  label_og <- levels(ordered_df$label)

  frequencies_same <- frequencies %>% order_same(ordered_df)
  label_same <- levels(frequencies_same$label)

  expect_equal(label_og, label_same)
})


test_that("Grouped frequencies", {
  frequencies <- data.frame(
    group_var = c(
      rep('One', 4),
      rep('Two', 4),
      rep('Three', 4)
    ),
    label = c(
      'Brand 1',
      'Brand 3',
      'Brand 2',
      'Brand 4'
    ),
    result = rep(c(1:4),3)
  )
  ordered_df <-  frequencies %>%
    dplyr::mutate(
      label = forcats::fct_inorder(label)
    ) %>%
    dplyr::mutate(
      group_var = forcats::fct_inorder(group_var)
    )
  label_og <- levels(ordered_df$label)
  group_og <- levels(ordered_df$group_var)

  frequencies_same <- frequencies %>% order_same(ordered_df)
  label_same <- levels(frequencies_same$label)
  group_same <- levels(frequencies_same$group_var)


  expect_equal(label_og, label_same)
  expect_equal(group_og, group_same)
})



# Arguments ---------------------------------------------------------------

test_that("group_var argument", {
  frequencies <- data.frame(
    groupedy_group_group = c(
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
  )
  ordered_df <-  frequencies %>%
    dplyr::mutate(
      label = forcats::fct_inorder(label)
    ) %>%
    dplyr::mutate(
      group_var = forcats::fct_inorder(groupedy_group_group)
    )
  label_og <- levels(ordered_df$label)
  group_og <- levels(ordered_df$group_var)

  frequencies_same <- frequencies %>% order_same(ordered_df, group_var = groupedy_group_group)
  label_same <- levels(frequencies_same$label)
  group_same <- levels(frequencies_same$group_var)


  expect_equal(label_og, label_same)
  expect_equal(group_og, group_same)
})

