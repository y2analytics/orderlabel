
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
      'What is your favorite color? ... Blue',
      'What is your favorite color? ... Green',
      'What is your favorite color? ... Yellow',
      'What is your favorite color? ... Purple',
      'What is your favorite color? ... Orange'
    )
  )

  test <- preamble_rm(frequencies, before_symbol = '\\. ')
  prompt_vector <- test$prompt
  correct_answer <- c("Blue", "Green", "Yellow", "Purple", "Orange")

  expect_equal(prompt_vector, correct_answer)
})


test_that("preamble_rm - different variable", {
  frequencies <- tibble::tibble(
    label = c('Selected','Selected','Selected','Selected','Selected'),
    result = c(.25, .15, .20, .10, .30),
    prompt2 = c(
      'What is your favorite color? - Blue',
      'What is your favorite color? - Green',
      'What is your favorite color? - Yellow',
      'What is your favorite color? - Purple',
      'What is your favorite color? - Orange'
    )
  )

  test <- preamble_rm(frequencies, var = prompt2)
  prompt_vector <- test$prompt2
  correct_answer <- c("Blue", "Green", "Yellow", "Purple", "Orange")

  expect_equal(prompt_vector, correct_answer)
})

test_that("preamble_rm - white space", {
  frequencies <- tibble::tibble(
    label = c('Selected'),
    result = c(.25),
    prompt1 = c('What is your favorite color? /n - Blue'),
    prompt2 = c('What is your favorite color? /n- Blue'),
    prompt3 = c('What is your favorite color? /n/n/n - Blue'),
    prompt4 = c('What is your favorite color? /t - Blue'),
    prompt5 = c('What is your favorite color?  -  Blue')
  )

  test1 <- preamble_rm(frequencies, var = prompt1) %>% dplyr::pull(prompt1)
  test2 <- preamble_rm(frequencies, var = prompt2) %>% dplyr::pull(prompt2)
  test3 <- preamble_rm(frequencies, var = prompt3) %>% dplyr::pull(prompt3)
  test4 <- preamble_rm(frequencies, var = prompt4) %>% dplyr::pull(prompt4)
  test5 <- preamble_rm(frequencies, var = prompt5) %>% dplyr::pull(prompt5)
  correct_answer <- c("Blue")

  expect_equal(test1, correct_answer)
  expect_equal(test2, correct_answer)
  expect_equal(test3, correct_answer)
  expect_equal(test4, correct_answer)
  expect_equal(test5, correct_answer)
})
