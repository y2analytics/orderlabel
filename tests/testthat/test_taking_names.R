
### unlabelled vars
test_that("unlabelled vars", {
  df <- data.frame(
    Q1 = c(1:5),
    Q2 = rep(.2, 5),
    QTRAILS = rep(100, 5)
  ) %>% taking_names()

  test <- tibble::tibble(
    name = c('Q1', 'Q2', 'QTRAILS'),
    label = c('NULL', 'NULL', 'NULL')
  ) %>% dplyr::mutate_all(.funs = ~as.character(.))

  expect_equal(df, test)
})

### vars with question wording
test_that("labelled vars", {
  df <- data.frame(
    Q1 = c(1:5),
    Q2 = rep(.2, 5),
    QTRAILS = rep(100, 5)
  )
  labelled::var_label(df$Q1) <- 'This is question 1?'
  labelled::var_label(df$Q2) <- 'And this is question 2?'
  labelled::var_label(df$QTRAILS) <- 'Are we done?'
  test <- taking_names(df)
  test2 <- tibble::tibble(
    name = c('Q1', 'Q2', 'QTRAILS'),
    label = c('This is question 1?', 'And this is question 2?', 'Are we done?')
  ) %>% dplyr::mutate_all(.funs = ~as.character(.))

  expect_equal(test, test2)
})
