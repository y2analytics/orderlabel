#### Executive Function ####
#' Auto order a freqs table to match another freqs table
#'
#' Takes a dataframe (frequencies) and orders it in the same order as another frequencies table in your R environment called "ordered_df".
#' @param dataset The name of the data frame for the function to modify, usually piped in after running freqs
#' @param orders DEFAULT = ordered_df; First create an ordered dataframe called ordered_df using order_label. Your new dataframe created using order_same will have the "label" and "group_var" columns in the same order as the dataframe specified in this argument.
#' @param group_var DEFAULT = 'NULL'. Leave as default if there is no grouping variable or if the grouping variable is called group_var. Otherwise, specify the group_var here - not in quotes. If you specify a variable name, this variable will be converted to the new "group_var" column, and the data will be ordered by that column.
#' @keywords order label equal same
#' @export
#' @examples
#' # The original frequencies, put in descending order of the result
#' frequencies <- tibble::tibble(
#'   label = c('Brand 1', 'Brand 2', 'Brand 3', 'Brand 4', 'Brand 5'),
#'   result = c(.25, .15, .20, .10, .30),
#'   value = c(1, 2, 3, 4, 5),
#'   group_var = rep('Group A', 5)
#' ) %>% order_label(group_var = group_var)
#' ordered_df <- frequencies
#'
#' # The second frequencies that you want to be ordered the same as the original
#' frequencies <- tibble::tibble(
#'   label = c('Brand 1', 'Brand 2', 'Brand 3', 'Brand 4', 'Brand 5'),
#'   result = c(.30, .10, .15, .20, .25),
#'   value = c(1, 2, 3, 4, 5),
#'   group_var = rep('Group B', 5)
#' ) %>% order_same()

order_same <- function(
    dataset,
    orders = ordered_df,
    group_var = 'NULL'
) {
  label_orders <- purrr::as_vector(orders$label) %>% levels()
  group_quoed <- rlang::enquo(group_var)
  group_character <- rlang::quo_name(group_quoed)

  if (is.null(label_orders)) {
    stop('The "label" variable in your "orders" data frame is not factored in a specific order. Please order your "orders" data frame before proceeding.')
  }

  # run ordering functions
  if (any(names(orders) == 'group_var') == TRUE |
      group_character != 'NULL'
  ) {

    if (any(names(orders) == 'group_var') == FALSE) {
      stop('If using the group_var argument, the data frame from the "orders" argument must have a column named "group_var". This will be the column by which your new data frame is ordered.')
    }

    dataset <- create_group_var(dataset, group_quoed, group_character)
    group_orders <- purrr::as_vector(orders$group_var) %>% levels()

    if (is.null(group_orders)) {
      stop('The "group_var" variable in your "orders" data frame is not factored in a specific order. Please order your "orders" data frame before proceeding.')
    }

    dataset <- grouped_vector(dataset, label_flag1 = label_orders, group_flag1 = group_orders)
  } else{ # NOT grouped
    dataset <- ungrouped_vector(dataset, label_flag1 = label_orders)
  }
  return(dataset)
}


#### ***** Hidden Functions ***** ####
#### Create group_var ####
create_group_var <- function(
    dataset,
    group_quoed,
    group_character
    ) {
  if (group_character != 'NULL' & group_character != 'group_var') {
  dataset <- dataset %>%
    dplyr::rename(
      group_var = !!group_quoed
    )
  }
  dataset <- dataset
}


#### Grouped ####
grouped_vector <- function(
  dataset,
  label_flag1,
  group_flag1
) {
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


