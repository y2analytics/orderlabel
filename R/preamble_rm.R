#### preamble_rm Function ####
#' Auto remove question preambles for multiple choice/matrix questions
#'
#' Takes a dataframe (frequencies) and removes the usual question preambles so that you are left with only the labels you care about.
#' For example, "What is your favorite color? - Blue" becomes "Blue"
#' @param dataset The name of the data frame for the function to modify, usually piped in after running freqs. You almost never need any arguments in this function.
#' @param var DEFAULT = prompt; If you use the prompt = T argument in freqs(), this default should be perfect for you.
#' @param before_symbol DEFAULT = "- "; preamble_rm will remove everything before this symbol or string of symbols
#' @importFrom rlang ":="
#' @export
#' @examples
#' frequencies <- tibble::tibble(
#'   label = c('Selected','Selected','Selected','Selected','Selected'),
#'   result = c(.25, .15, .20, .10, .30),
#'   prompt = c(
#'   'What is your favorite color? - Blue',
#'   'What is your favorite color? - Green',
#'   'What is your favorite color? - Yellow',
#'   'What is your favorite color? - Purple',
#'   'What is your favorite color? - Orange'
#'   ),
#'   prompt2 = c(
#'   'What is your favorite color? \nBlue',
#'   'What is your favorite color? \nGreen',
#'   'What is your favorite color? \nYellow',
#'   'What is your favorite color? \nPurple',
#'   'What is your favorite color? \nOrange'
#'   )
#' )
#'
#' preamble_rm(frequencies)
#' preamble_rm(frequencies, var = prompt2, before_symbol = '\n')
#' \dontrun{
#' frequencies %>% preamble_rm()
#' }

preamble_rm <- function(
  dataset,
  var = prompt,
  before_symbol = '- '
) {
  combined_remove_symbol <- stringr::str_c('.*', before_symbol)

  dataset <- dataset %>%
    dplyr::mutate(
      '{{var}}' := as.character({{var}}),
      '{{var}}' := stringr::str_remove({{var}}, combined_remove_symbol)
    )
  return(dataset)
}

