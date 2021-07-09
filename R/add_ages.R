# add_ages primary function -----------------------------------------------
#' Add age and census age groups to a data frame
#'
#' Takes a data frame with a year born variable or age variable and creates four new variables:
#'
#' 1. year_born_numeric
#' 2. age_numeric
#' 3. census_age_groups
#' 4. census_age_groups_6 (same as census_age_groups, but with split 18-34 into 18-24 & 25-34)
#' @param dataset The name of the data frame for the function to modify, usually piped into your main data frame.
#' @param year_born_var NO DFAULT; You may either provide a year_born_var or an age_var, not both
#' @param age_var NO DFAULT
#' @export
#' @examples
#' responses <- tibble::tibble(
#'   year_born = c(
#'   2000,
#'   2001
#'   ),
#'   age = c(
#'   20,
#'   19
#'   )
#' )
#'
#' add_ages(responses, year_born_var = year_born)
#' add_ages(responses, age_var = age)

add_ages <- function(
  dataset,
  year_born_var,
  age_var
) {
  if(!missing(year_born_var) & !missing(age_var))
    stop("You specified both year_born_var and age_var, please specify only one")
  current_year <- get_current_year()

  # User specified year_born_var
  if(!missing(year_born_var)){
    dataset <- year_born_var_specified(dataset, {{ year_born_var }}, current_year)
    }

  # User specified age_var
  if(!missing(age_var)){
    dataset <- age_var_specified(dataset, {{ age_var }}, current_year)
    }

  dataset <- create_age_groups(dataset)
  return(dataset)
}



# private functions -------------------------------------------------------
### Step 1 - get current date
get_current_year <- function(){
  date <- Sys.Date()
  current_year <- stringr::str_remove_all(date, '-.*') %>% as.numeric()
}


### Step 2 - create 1.year_born_numeric and 2.age_numeric
year_born_var_specified <- function(
  dataset,
  year_born_var,
  current_year
){
  dataset <- dataset %>%
    dplyr::mutate(
      year_born_numeric = forcats::as_factor({{ year_born_var }}) %>%
        as.character() %>%
        as.numeric(),
      age_numeric = current_year - .data$year_born_numeric
    )
}

age_var_specified <- function(
  dataset,
  age_var,
  current_year
){
  dataset <- dataset %>%
    dplyr::mutate(
      age_numeric = forcats::as_factor({{ age_var }}) %>%
        as.character() %>%
        as.numeric(),
      year_born_numeric = current_year - .data$age_numeric
    )
}


### Step 3 - census_age_groups
create_age_groups <- function(dataset){
  dataset <- dataset %>%
    dplyr::mutate(
      census_age_groups = dplyr::case_when(
        # If born in 2012, could still be 17 (as of 2020)
        # So there could be SOME 17 year olds, but not all of them
        # To be simple, we'll just say it starts at 18
        .data$age_numeric < 18 ~ NA_character_,
        .data$age_numeric <= 34 ~ '18-34',
        .data$age_numeric <= 44 ~ '35-44',
        .data$age_numeric <= 54 ~ '45-54',
        .data$age_numeric <= 64 ~ '55-64',
        .data$age_numeric >= 65 ~ '65+'
      ) %>%
        # suppressWarnings added for trivial missing levels
        suppressWarnings(
          forcats::fct_relevel(
          '18-34',
          '35-44',
          '45-54',
          '55-64',
          '65+'
          )
        ),
      census_age_groups_6 = dplyr::case_when(
        .data$age_numeric < 18 ~ NA_character_,
        .data$age_numeric <= 24 ~ '18-24',
        .data$age_numeric <= 34 ~ '25-34',
        .data$age_numeric <= 44 ~ '35-44',
        .data$age_numeric <= 54 ~ '45-54',
        .data$age_numeric <= 64 ~ '55-64',
        .data$age_numeric >= 65 ~ '65+'
      ) %>%
        # suppressWarnings added for trivial missing levels
        suppressWarnings(
          forcats::fct_relevel(
          '18-24',
          '25-34',
          '35-44',
          '45-54',
          '55-64',
          '65+'
          )
        )
    )
}

