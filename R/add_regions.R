#### add_regions Function ####
#' Add a "census_region" variable based on states
#'
#' Takes a data frame with a state variable and uses those states to categorize US census regions
#' @param dataset The name of the data frame for the function to modify, usually piped into your main data frame.
#' @param state_var NO DEFAULT; The state variable on which census regions will be based. Can be state name or state abbreviations
#' @param new_name DEFAULT = census_region; the name for your new census region variable, as an object not a "string"
#' @export
#' @examples
#' responses <- tibble::tibble(
#'   state = c(
#'   'Virginia',
#'   'UTAH',
#'   'New York',
#'   'alaska',
#'   'Washington DC'
#'   )
#' )
#'
#' add_regions(responses, state_var = state)

add_regions <- function(
  dataset,
  state_var,
  new_name = census_region
) {
  census_region <- NULL
  dataset <- dataset %>%
    dplyr::mutate(
      state_new_var = {{ state_var }},
      state_new_var = forcats::as_factor(.data$state_new_var) %>%
        as.character() %>%
        stringr::str_to_title(),
      "{{new_name}}" := dplyr::case_when(
        # Full State Names
        .data$state_new_var == 'Alabama'   ~   'South',
        .data$state_new_var == 'Alaska'    ~   'West',
        .data$state_new_var == 'Arizona'   ~   'West',
        .data$state_new_var == 'Arkansas'  ~   'South',
        .data$state_new_var == 'California'    ~   'West',
        .data$state_new_var == 'Colorado'  ~   'West',
        .data$state_new_var == 'Connecticut'   ~   'Northeast',
        .data$state_new_var == 'Delaware'  ~   'South',
        .data$state_new_var == 'Florida'   ~   'South',
        .data$state_new_var == 'Georgia'   ~   'South',
        .data$state_new_var == 'Hawaii'    ~   'West',
        .data$state_new_var == 'Idaho' ~   'West',
        .data$state_new_var == 'Illinois'  ~   'Midwest',
        .data$state_new_var == 'Indiana'   ~   'Midwest',
        .data$state_new_var == 'Iowa'  ~   'Midwest',
        .data$state_new_var == 'Kansas'    ~   'Midwest',
        .data$state_new_var == 'Kentucky'  ~   'South',
        .data$state_new_var == 'Louisiana' ~   'South',
        .data$state_new_var == 'Maine' ~   'Northeast',
        .data$state_new_var == 'Maryland'  ~   'South',
        .data$state_new_var == 'Massachusetts' ~   'Northeast',
        .data$state_new_var == 'Michigan'  ~   'Midwest',
        .data$state_new_var == 'Minnesota' ~   'Midwest',
        .data$state_new_var == 'Mississippi'   ~   'South',
        .data$state_new_var == 'Missouri'  ~   'Midwest',
        .data$state_new_var == 'Montana'   ~   'West',
        .data$state_new_var == 'Nebraska'  ~   'Midwest',
        .data$state_new_var == 'Nevada'    ~   'West',
        .data$state_new_var == 'New Hampshire' ~   'Northeast',
        .data$state_new_var == 'New Jersey'    ~   'Northeast',
        .data$state_new_var == 'New Mexico'    ~   'West',
        .data$state_new_var == 'New York'  ~   'Northeast',
        .data$state_new_var == 'North Carolina'    ~   'South',
        .data$state_new_var == 'North Dakota'  ~   'Midwest',
        .data$state_new_var == 'Ohio'  ~   'Midwest',
        .data$state_new_var == 'Oklahoma'  ~   'South',
        .data$state_new_var == 'Oregon'    ~   'West',
        .data$state_new_var == 'Pennsylvania'  ~   'Northeast',
        .data$state_new_var == 'Rhode Island'  ~   'Northeast',
        .data$state_new_var == 'South Carolina'    ~   'South',
        .data$state_new_var == 'South Dakota'  ~   'Midwest',
        .data$state_new_var == 'Tennessee' ~   'South',
        .data$state_new_var == 'Texas' ~   'South',
        .data$state_new_var == 'Utah'  ~   'West',
        .data$state_new_var == 'Vermont'   ~   'Northeast',
        .data$state_new_var == 'Virginia'  ~   'South',
        .data$state_new_var == 'Washington'    ~   'West',
        .data$state_new_var == 'West Virginia' ~   'South',
        .data$state_new_var == 'Wisconsin' ~   'Midwest',
        .data$state_new_var == 'Wyoming'   ~   'West',
        # Abbreviations
        .data$state_new_var == 'Al'   ~   'South',
        .data$state_new_var == 'Ak'    ~   'West',
        .data$state_new_var == 'Az'   ~   'West',
        .data$state_new_var == 'Ar'  ~   'South',
        .data$state_new_var == 'Ca'    ~   'West',
        .data$state_new_var == 'Co'  ~   'West',
        .data$state_new_var == 'Ct'   ~   'Northeast',
        .data$state_new_var == 'De'  ~   'South',
        .data$state_new_var == 'Fl'   ~   'South',
        .data$state_new_var == 'Ga'   ~   'South',
        .data$state_new_var == 'Hi'    ~   'West',
        .data$state_new_var == 'Id' ~   'West',
        .data$state_new_var == 'Il'  ~   'Midwest',
        .data$state_new_var == 'In'   ~   'Midwest',
        .data$state_new_var == 'Ia'  ~   'Midwest',
        .data$state_new_var == 'Ks'    ~   'Midwest',
        .data$state_new_var == 'Ky'  ~   'South',
        .data$state_new_var == 'La' ~   'South',
        .data$state_new_var == 'Me' ~   'Northeast',
        .data$state_new_var == 'Md'  ~   'South',
        .data$state_new_var == 'Ma' ~   'Northeast',
        .data$state_new_var == 'Mi'  ~   'Midwest',
        .data$state_new_var == 'Mn' ~   'Midwest',
        .data$state_new_var == 'Ms'   ~   'South',
        .data$state_new_var == 'Mo'  ~   'Midwest',
        .data$state_new_var == 'Mt'   ~   'West',
        .data$state_new_var == 'Ne'  ~   'Midwest',
        .data$state_new_var == 'Nv'    ~   'West',
        .data$state_new_var == 'Nh' ~   'Northeast',
        .data$state_new_var == 'Nj'    ~   'Northeast',
        .data$state_new_var == 'Nm'    ~   'West',
        .data$state_new_var == 'Ny'  ~   'Northeast',
        .data$state_new_var == 'Nc'    ~   'South',
        .data$state_new_var == 'Nd'  ~   'Midwest',
        .data$state_new_var == 'Oh'  ~   'Midwest',
        .data$state_new_var == 'Ok'  ~   'South',
        .data$state_new_var == 'Or'    ~   'West',
        .data$state_new_var == 'Pa'  ~   'Northeast',
        .data$state_new_var == 'Ri'  ~   'Northeast',
        .data$state_new_var == 'Sc'    ~   'South',
        .data$state_new_var == 'Sd'  ~   'Midwest',
        .data$state_new_var == 'Tn' ~   'South',
        .data$state_new_var == 'Tx' ~   'South',
        .data$state_new_var == 'Ut'  ~   'West',
        .data$state_new_var == 'Vt'   ~   'Northeast',
        .data$state_new_var == 'Va'  ~   'South',
        .data$state_new_var == 'Wa'    ~   'West',
        .data$state_new_var == 'Wv' ~   'South',
        .data$state_new_var == 'Wi' ~   'Midwest',
        .data$state_new_var == 'Wy'   ~   'West',
        # Washington DC
        stringr::str_detect(.data$state_new_var, 'District') ~ 'South',
        stringr::str_detect(.data$state_new_var, 'Dc') ~ 'South',
        stringr::str_detect(.data$state_new_var, 'D.') ~ 'South'
      )
    ) %>%
    dplyr::select(-'state_new_var')
  return(dataset)
}

