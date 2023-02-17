
test_that("preamble_rm - all defaults", {
  frequencies <- tibble::tibble(
    label = c('Selected','Selected','Selected','Selected','Selected'),
    result = c(.25, .15, .20, .10, .30),
    prompt = c(
      'What is your favorite color? - Blue',
      'What is your favorite color? - Green',
      'What is your favorite color? - Yellow',
      'What is your favorite color? - Purple',
      'What is your favorite color? - Orange'
    ),
    prompt2 = c(
      'What is your favorite color? \nBlue',
      'What is your favorite color? \nGreen',
      'What is your favorite color? \nYellow',
      'What is your favorite color? \nPurple',
      'What is your favorite color? \nOrange'
    )
  )

  test <- preamble_rm(frequencies)
  prompt_vector <- test$prompt
  correct_answer <- c("Blue", "Green", "Yellow", "Purple", "Orange")

  expect_equal(prompt_vector, correct_answer)
})


test_that("preamble_rm - different symbols", {
  frequencies <- tibble::tibble(
    label = c('Selected','Selected','Selected','Selected','Selected'),
    result = c(.25, .15, .20, .10, .30),
    prompt = c(
      'What is your favorite color? - Blue',
      'What is your favorite color? - Green',
      'What is your favorite color? - Yellow',
      'What is your favorite color? - Purple',
      'What is your favorite color? - Orange'
    ),
    prompt2 = c(
      'What is your favorite color? \nBlue',
      'What is your favorite color? \nGreen',
      'What is your favorite color? \nYellow',
      'What is your favorite color? \nPurple',
      'What is your favorite color? \nOrange'
    )
  )
  new_df <- frequencies %>% dplyr::mutate(prompt = prompt2)

  test <- preamble_rm(new_df, before_symbol = '\n')
  prompt_vector <- test$prompt
  correct_answer <- c("Blue", "Green", "Yellow", "Purple", "Orange")

  expect_equal(prompt_vector, correct_answer)
})


test_that("preamble_rm - different variable", {
  frequencies <- tibble::tibble(
    label = c('Selected','Selected','Selected','Selected','Selected'),
    result = c(.25, .15, .20, .10, .30),
    prompt = c(
      'What is your favorite color? - Blue',
      'What is your favorite color? - Green',
      'What is your favorite color? - Yellow',
      'What is your favorite color? - Purple',
      'What is your favorite color? - Orange'
    ),
    prompt2 = c(
      'What is your favorite color? \nBlue',
      'What is your favorite color? \nGreen',
      'What is your favorite color? \nYellow',
      'What is your favorite color? \nPurple',
      'What is your favorite color? \nOrange'
    )
  )
  new_df <- frequencies %>% dplyr::mutate(prompt2 = prompt)

  test <- preamble_rm(new_df, var = prompt2)
  prompt_vector <- test$prompt2
  correct_answer <- c("Blue", "Green", "Yellow", "Purple", "Orange")

  expect_equal(prompt_vector, correct_answer)
})
