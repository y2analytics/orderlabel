#### taking_names Function ####
#' Create a dataframe of varaible names and labels
#'
#' Takes a dataset (responses) and creates a new df with two columns: 1) the variable names of your original dataset, 2) the labels of those variables (question wording from a sav/rds file)
#' @param dataset DEFAULT = responses; The name of the data frame for the function to use, typically responses
#' @keywords name label
#' @export
#' @examples
#' names <- taking_names(mtcars)
#'
#' responses <- mtcars
#' names <- taking_names()
#

taking_names <- function(dataset = responses) {
  labels <- sapply(dataset, function(x) attr(x, "label"))
  tibble::tibble(name = names(labels),
         label = labels)
}

