#### other_rm Function ####
#' Auto change those pesky "Other please specify"s into "Other"
#'
#' Takes a dataframe (frequencies) and replaces the usual variations of "Other please specify" into Other. Also standardizes "None of the above" and "Prefer not to say" options. Additionally, removes all extra text after parantheses opening. Does this for the 'label', 'variable', and 'group_var' vars.
#' @param dataset The name of the data frame for the function to modify, usually piped in after running freqs. You almost never need any arguments in this function.
#' @param remove DEFAULT = FALSE; If set to TRUE, will remove categories of "Other" and "None of the above" from the frequencies
#' @keywords other none extra
#' @export
#' @examples
#' frequencies <- tibble(
#'   label = c('Brand 1', 'Brand 2', 'Brand 3', 'Other (please specify)', 'None of the above brands...'),
#'   result = c(.25, .15, .20, .10, .30)
#' )
#'
#' frequencies
#' frequencies %>% other_rm()
#' frequencies %>% other_rm(T)

other_rm <- function(
  dataset,
  remove = FALSE
) {
  dataset <- other_rm_label(dataset)
  dataset <- other_rm_variable(dataset)
  dataset <- other_rm_group(dataset)
  dataset <- remove_function(dataset, remove)
  return(dataset)
}

#### hidden functions ####
# label
other_rm_label <- function(dataset){
  dataset <- dataset %>%
    dplyr::mutate(
      label = as.character(label),
      label = dplyr::case_when(
        stringr::str_detect(label, stringr::regex('prefer not to', ignore_case = T)) == T ~ 'Prefer not to say',
        stringr::str_detect(label, stringr::regex('please specify', ignore_case = T)) == T ~ 'Other',
        stringr::str_detect(label, stringr::regex('none of the', ignore_case = T)) == T ~ 'None of the above',
        label == 'None' ~ 'None of the above',
        T ~ label
      ),
      label = stringr::str_remove_all(label, ' \\(.*')
    )
}

# variable
other_rm_variable <- function(dataset){
  if(any(names(dataset) == 'variable') == T){
    dataset <- dataset %>%
      dplyr::mutate(
        variable = as.character(variable),
        variable = dplyr::case_when(
          stringr::str_detect(variable, stringr::regex('prefer not to', ignore_case = T)) == T ~ 'Prefer not to say',
          stringr::str_detect(variable, stringr::regex('please specify', ignore_case = T)) == T ~ 'Other',
          stringr::str_detect(variable, stringr::regex('none of the', ignore_case = T)) == T ~ 'None of the above',
          variable == 'None' ~ 'None of the above',
          T ~ variable
        ),
        variable = stringr::str_remove_all(variable, ' \\(.*')
      )
  } else{
    dataset <- dataset
  }
}

# group_var
other_rm_group <- function(dataset){
  if(any(names(dataset) == 'group_var') == T){
    dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        group_var = as.character(group_var),
        group_var = dplyr::case_when(
          stringr::str_detect(group_var, stringr::regex('prefer not to', ignore_case = T)) == T ~ 'Prefer not to say',
          stringr::str_detect(group_var, stringr::regex('please specify', ignore_case = T)) == T ~ 'Other',
          stringr::str_detect(group_var, stringr::regex('none of the', ignore_case = T)) == T ~ 'None of the above',
          group_var == 'None' ~ 'None of the above',
          T ~ group_var
        ),
        group_var = stringr::str_remove_all(group_var, ' \\(.*')
      )
  } else{
    dataset <- dataset
  }
}

# remove_function
remove_function <- function(dataset, remove){
  if(remove == T){
    dataset <- dataset %>%
      dplyr::filter_all(
        ~str_detect(., 'Other') == F
      ) %>%
      dplyr::filter_all(
        ~str_detect(., 'None of the above') == F
      )  %>%
      dplyr::filter_all(
        ~str_detect(., 'Prefer not to ') == F
      )
  } else{
    dataset <- dataset
  }
}
