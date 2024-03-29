#### Final order_topline Function ####
#' Add \%s to a topline report
#'
#' Takes a dataframe (frequencies) and for the first Y (result) of every X (variable), adds a \%. Also changes all 0 to <1 if n >=1
#' @param dataset The name of the data frame that the mscharts pulls from, usually piped in after running freqs. Please note that the variable column must be "variable" and the percentage column must be "result"
#' @param whole_numbers DEFAULT = If you have only variables that are percentages, and no whole number variables, you can leave this argument blank. Otherwise, add the names of the variables that are not percentages here separated by a "|" (OR sign). You do not have to type out the whole variable name/each option of a multiple select. A unique portion of the var name will work as well because this argument uses str_detect().
#' @keywords topline percent label
#' @export
#' @examples
#' \dontrun{
#' frequencies <- freqs(mtcars, vs, am, gear, carb)
#' order_topline(frequencies)
#'
#' frequencies <- freqs(mtcars, mpg, cyl, disp, stat = 'mean')
#' order_topline(frequencies, 'mpg|cyl|disp')
#' }


order_topline <- function(
  dataset,
  whole_numbers = 'place your variable names here with a | (OR sign) between them'
) {
  dataset <- dataset %>%
    var_sep() %>%
    add_percent() %>%
    add_lessthan() %>%
    whole_numbers(whole_numbers)

  return(dataset)
}



#### ***** Hidden order_topline Functions ***** ####
#### var_sep ####
var_sep <- function(dataset) {
  dataset %>%
    dplyr::mutate(
      var = .data$variable
    ) %>%
    tidyr::separate(
      .data$var,
      into = stringr::str_c('variable', 1:4),
      sep = "_"
    ) %>%
    dplyr::mutate(
      variable3 = dplyr::case_when(
        is.na(variable4) & stringr::str_detect(variable3, "[A-Za-z]") == FALSE ~ NA_character_,
        TRUE ~ variable3
      ),
      variable2 = dplyr::case_when(
        is.na(variable3) & stringr::str_detect(variable2, "[A-Za-z]") == FALSE ~ NA_character_,
        TRUE ~ variable2
      ),
      sort_var = dplyr::case_when(
        is.na(variable2) ~ variable1,
        is.na(variable3) ~ stringr::str_c(variable1, "_", variable2),
        is.na(variable4) ~ stringr::str_c(variable1, "_", variable2, '_', variable3),
        TRUE ~ stringr::str_c(variable1, "_", variable2, '_', variable3, '_', variable4)
      )
    )
}

#### add_percent ####
add_percent <- function(dataset) {
  dataset %>%
    dplyr::group_by(.data$sort_var) %>%
    dplyr::mutate(
      percent_label = dplyr::case_when(
        label == label[1] & variable == variable[1] ~ stringr::str_c(.data$result * 100, '%'),
        TRUE ~ stringr::str_c(.data$result * 100)
      )
    )
}

#### add_lessthan ####
add_lessthan <- function(dataset) {
  dataset %>%
    dplyr::mutate(
      percent_label = dplyr::case_when(
        .data$percent_label =='0%' & .data$n >= 1 ~ '<1%',
        .data$percent_label == '0' & .data$n >= 1 ~ '<1',
        TRUE ~ .data$percent_label
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -'variable1',
      -'variable2',
      -'variable3',
      -'variable4',
      -'sort_var'
    )
}

#### whole numbers ####
whole_numbers <- function(
  dataset,
  whole_numbers
) {
  dataset %>%
    dplyr::mutate(
      percent_label = dplyr::case_when(
        stringr::str_detect(variable, whole_numbers) ~ as.character(result),
        TRUE ~ percent_label
      )
    )
}

