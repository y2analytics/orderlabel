#### Executive Function ####
#' Auto order a freqs table to match another freqs table
#'
#' Takes a dataframe (frequencies) and orders it in the same order as another frequencies table in your R environment called "ordered_df".
#' @param dataset The name of the data frame for the function to modify, usually piped in after running freqs
#' @param orders DEFAULT = ordered_df; First create an ordered dataframe called ordered_df using order_label. Your new dataframe created using order_same will have variables and groups in the same order as ordered_df
#' @param group_var DEFAULT = 'NULL'. Leave as default if there is no grouping variable or if the grouping variable is valled "group_var". Otherwise, specify the group_var here in quotes
#' @keywords order label equal same
#' @export
#' @examples
#' # The original frequencies, put in descending order of the result
#' \dontrun{
#' frequencies <- tibble::tibble(
#'   label = c('Brand 1', 'Brand 2', 'Brand 3', 'Brand 4', 'Brand 5'),
#'   result = c(.25, .15, .20, .10, .30),
#'   value = c(1, 2, 3, 4, 5),
#'   group_var = rep('Group A', 5)
#' ) %>% order_label()
#' ordered_df <- frequencies
#'
#' # The second frequencies that you want to be ordered the same as the original
#' frequencies <- tibble::tibble(
#'   label = c('Brand 1', 'Brand 2', 'Brand 3', 'Brand 4', 'Brand 5'),
#'   result = c(.30, .10, .15, .20, .25),
#'   value = c(1, 2, 3, 4, 5),
#'   group_var = rep('Group B', 5)
#' ) %>% order_same()
#' }

order_same <- function(
  dataset,
  orders = ordered_df,
  group_var = 'NULL'
  ) {
  label_flag <- purrr::as_vector(orders$label) %>% levels()

  # run ordering functions
 if(any(names(orders) == 'group_var') == TRUE |
    group_var != 'NULL'
   ) {
   dataset <- create_group_var(dataset, group_var)
   group_flag <- purrr::as_vector(orders$group_var) %>% levels()
   dataset <- grouped_vector(dataset, label_flag1 = label_flag, group_flag1 = group_flag)
 } else{ # NOT grouped
   dataset <- ungrouped_vector(dataset, label_flag1 = label_flag)
 }
  return(dataset)
}


#### ***** Hidden Functions ***** ####
#### Create group_var ####
create_group_var <- function(dataset, group_var){
  if(group_var != 'NULL'){
    group_var_old <- dplyr::enquo(group_var)

  dataset <- dataset %>%
    dplyr::rename(
      group_var = !!group_var_old
    )
  }
  dataset <- dataset
}


#### Grouped ####
grouped_vector <- function(
  dataset,
  label_flag1,
  group_flag1
){
  dataset %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      # first arrange by groups
      group_var = forcats::fct_relevel(
        .data$group_var,
        group_flag1
      ),
      # then arrange by labels, now in order of both
      label = forcats::fct_relevel(
        .data$label,
        label_flag1
      )
    ) %>%
    dplyr::mutate(
      label = forcats::fct_inorder(.data$label)
    ) %>%
    dplyr::mutate(
      group_var = forcats::fct_inorder(.data$group_var)
    ) %>%
    dplyr::mutate(
      percent_label =  stringr::str_c(.data$result * 100)
    )
}


#### ungrouped ####
ungrouped_vector <- function(
  dataset,
  label_flag1
) {
  dataset %>%
    dplyr::arrange(
      label = forcats::fct_relevel(
        .data$label,
        label_flag1
      )
    ) %>%
    dplyr::mutate(
      label = forcats::fct_inorder(.data$label)
    ) %>%
    dplyr::mutate(
      percent_label = stringr::str_c(.data$result * 100)
    )
}


