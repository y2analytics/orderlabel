#### other_rm Function ####
#' Auto change "Other please specify"s into "Other"
#'
#' Takes a dataframe (frequencies) and replaces the usual variations of "Other please specify" into Other. Also standardizes "None of the above" and "Prefer not to say" options. Additionally, removes all extra text after parentheses opening. Does this for the 'label', 'variable', and 'group_var' vars.
#' @param dataset The name of the data frame for the function to modify, usually piped in after running freqs. You almost never need any arguments in this function.
#' @param var DEFAULT = NULL; If kept at NULL, it will already remove parenthetical text in 'label', 'variable', and 'group_var' vars. You can add an additional variable by specifying it here.
#' @param remove DEFAULT = FALSE; If set to TRUE, will remove categories of "Other" and "None of the above" from the frequencies
#' @keywords other none extra
#' @export
#' @examples
#' frequencies <- tibble::tibble(
#'   label = c(
#'   'Brand 1',
#'   'Brand 2',
#'   'Brand 3',
#'   'Other (please specify)',
#'   'None of the above brands...'
#'   ),
#'   result = c(.25, .15, .20, .10, .30),
#'   other_var = c('x', 'y', 'z', 'z (test)', 'None')
#' )
#'
#' frequencies %>% other_rm(remove = TRUE)
#' frequencies %>% other_rm(var = other_var)

other_rm <- function(
    dataset,
    var = NULL,
    remove = FALSE
) {
  variable_quoed <- dplyr::enquo(var)
  variable_char <- dplyr::quo_name(variable_quoed)
  vars_to_edit <- c('variable', 'prompt', 'group_var', 'label', variable_char)

  for(i in vars_to_edit) {
    if (any(names(dataset) == i) == TRUE) {
      symb_var <- rlang::sym(i)
      dataset <- dataset %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          '{{symb_var}}' := as.character({{ symb_var }}),
          '{{symb_var}}' := stringr::str_squish({{ symb_var }}),
          '{{symb_var}}' := stringr::str_remove_all({{ symb_var }}, '/n'),
          '{{symb_var}}' := dplyr::case_when(
            stringr::str_detect({{ symb_var }}, stringr::regex('prefer not to', ignore_case = TRUE)) == TRUE ~ 'Prefer not to say',
            stringr::str_detect({{ symb_var }}, stringr::regex('please specify', ignore_case = TRUE)) == TRUE ~ 'Other',
            stringr::str_detect({{ symb_var }}, stringr::regex('none of the', ignore_case = TRUE)) == TRUE ~ 'None of the above',
            {{ symb_var }} == 'None' ~ 'None of the above',
            TRUE ~ {{ symb_var }}
          ),
          '{{symb_var}}' := stringr::str_remove_all({{ symb_var }}, ' \\(.*')
        )
    } else {
      dataset <- dataset
    }
  }

  dataset <- remove_argument(dataset, remove)
  return(dataset)
}

#### hidden functions ####
# remove_function
remove_argument <- function(
    dataset,
    remove
) {
  if (remove == TRUE) {
    dataset <- dataset %>%
      dplyr::filter_all(
        ~stringr::str_detect(., 'Other') == FALSE
      ) %>%
      dplyr::filter_all(
        ~stringr::str_detect(., 'None of the above') == FALSE
      )  %>%
      dplyr::filter_all(
        ~stringr::str_detect(., 'Prefer not to ') == FALSE
      )
  } else {
    dataset <- dataset
  }
}
