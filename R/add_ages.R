# add_ages primary function -----------------------------------------------
#' Add age and census age groups to a data frame
#'
#' Takes a data frame with a year born variable or age variable and creates four new variables:
#'
#' 1. year_born_numeric
#' 2. age_numeric
#' 3. census_age_groups
#' 4. census_age_groups_6 (same as census_age_groups, but with split 18-34 into 18-24 & 25-34)
#' @param dataset The name of the data frame for the function to modify, usually piped into your main data frame
#' @param year_born_var NO DFAULT; You may either provide a year_born_var or an age_var, not both
#' @param age_var NO DFAULT
#' @param year_of_survey DEFAULT = Whatever the current year is. Used to calculate respondent ages. If you are using old survey data from a previous year, you can input the year here as a numeric value (e.g. 1994)
#' @param survey_date_var DEFAULT = 'NULL'. A date variable in the dataset that specifies when the survey was taken. If specified, ages will be calculated off this and not the year_of_survey argument. Especially helpful for trended data that spans multiple years
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
  age_var,
  year_of_survey = 0,
  survey_date_var = 'NULL'
) {
  variable_quoed <- rlang::enquo(survey_date_var)
  variable_char <- rlang::quo_name(variable_quoed)

  # Error #1
  if(variable_char != 'NULL') {
  class_of_survey_date_var <- dataset %>%
    dplyr::pull({{ survey_date_var }}) %>%
    class()

  if(
    class_of_survey_date_var[1] != 'Date' &
    class_of_survey_date_var[1] != 'POSIXct' &
    class_of_survey_date_var[1] != 'POSIXt'
    )
    stop(stringr::str_c('survey_date_var must be a type of date variable, not a ', class_of_survey_date_var[1]))
  }

  # Error #2
  if(!missing(year_born_var) & !missing(age_var))
    stop('You specified both year_born_var and age_var, please specify only one')

  # Error #3
  if(year_of_survey != 0 & variable_char != 'NULL')
    stop('You specified both year_of_survey and survey_date_var, please specify only one')

  calculated_year <- calculate_current_v_survey_date(
    dataset,
    variable_char,
    year_of_survey,
    {{ survey_date_var }}
    )

  # User specified year_born_var
  if(!missing(year_born_var)){
    dataset <- year_born_var_specified(dataset, {{ year_born_var }}, calculated_year)
    }

  # User specified age_var
  if(!missing(age_var)){
    dataset <- age_var_specified(dataset, {{ age_var }}, calculated_year)
    }

  dataset <- create_age_groups(dataset)
  return(dataset)
}



# private functions -------------------------------------------------------
### Step 1 - get current date or date of survey
get_current_year <- function(year_of_survey){
  if (year_of_survey == 0) {
  date <- Sys.Date()
  current_year <- stringr::str_remove_all(date, '-.*') %>% as.numeric()
  } else {
  current_year <- year_of_survey
  }
}

calculate_current_v_survey_date <- function(
    dataset,
    variable_char,
    year_of_survey,
    survey_date_var
  ) {
  if(variable_char == 'NULL') {
    calculated_year <- get_current_year(year_of_survey)
  } else {
    calculated_year <- dataset %>%
      dplyr::mutate(survey_year = lubridate::year({{ survey_date_var }})) %>%
      dplyr::pull()
  }
}


### Step 2 - create 1.year_born_numeric and 2.age_numeric
year_born_var_specified <- function(
  dataset,
  year_born_var,
  calculated_year
){
  dataset <- dataset %>%
    dplyr::mutate(
      year_born_numeric = forcats::as_factor({{ year_born_var }}) %>%
        as.character() %>%
        as.numeric(),
      age_numeric = calculated_year - .data$year_born_numeric
    )
}

age_var_specified <- function(
  dataset,
  age_var,
  calculated_year
){
  dataset <- dataset %>%
    dplyr::mutate(
      age_numeric = forcats::as_factor({{ age_var }}) %>%
        as.character() %>%
        as.numeric(),
      year_born_numeric = calculated_year - .data$age_numeric
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

