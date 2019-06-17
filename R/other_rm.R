#### other_rm Function ####
#' Auto change those pesky "Other please specify"s into "Other"
#'
#' Takes a dataframe (frequencies) and replaces the usual variations of "Other please specify" into Other. Also standardizes "None of the above" and "Prefer not to say" options. Additionally, removes all extra text after parantheses opening. Does this for both the 'label' and 'variable' vars.
#' @param dataset The name of the data frame for the function to modify, usually piped in after running freqs. You almost never need any arguments in this function.
#' @keywords other none extra
#' @export
#' @examples
#' frequencies %>% other_rm()
#

other_rm <- function(
  dataset
) {
  #Remove for 'label' var
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
  #Remove for 'variable' var
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


