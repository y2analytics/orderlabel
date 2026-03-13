# Add age and census age groups to a data frame

Takes a data frame with a year born variable or age variable and creates
four new variables:

## Usage

``` r
add_ages(
  dataset,
  year_born_var,
  age_var,
  year_of_survey = 0,
  survey_date_var = "NULL"
)
```

## Arguments

- dataset:

  The name of the data frame for the function to modify, usually piped
  into your main data frame

- year_born_var:

  NO DFAULT; You may either provide a year_born_var or an age_var, not
  both

- age_var:

  NO DFAULT

- year_of_survey:

  DEFAULT = Whatever the current year is. Used to calculate respondent
  ages. If you are using old survey data from a previous year, you can
  input the year here as a numeric value (e.g. 1994)

- survey_date_var:

  DEFAULT = 'NULL'. A date variable in the dataset that specifies when
  the survey was taken. If specified, ages will be calculated off this
  and not the year_of_survey argument. Especially helpful for trended
  data that spans multiple years

## Details

1\. year_born_numeric 2. age_numeric 3. census_age_groups 4.
census_age_groups_6 (same as census_age_groups, but with split 18-34
into 18-24 & 25-34)

## Examples

``` r
responses <- tibble::tibble(
  year_born = c(
  2000,
  2001
  ),
  age = c(
  20,
  19
  )
)

add_ages(responses, year_born_var = year_born)
#> # A tibble: 2 × 6
#>   year_born   age year_born_numeric age_numeric census_age_groups
#>       <dbl> <dbl>             <dbl>       <dbl> <chr>            
#> 1      2000    20              2000          26 18-34            
#> 2      2001    19              2001          25 18-34            
#> # ℹ 1 more variable: census_age_groups_6 <chr>
add_ages(responses, age_var = age)
#> # A tibble: 2 × 6
#>   year_born   age age_numeric year_born_numeric census_age_groups
#>       <dbl> <dbl>       <dbl>             <dbl> <chr>            
#> 1      2000    20          20              2006 18-34            
#> 2      2001    19          19              2007 18-34            
#> # ℹ 1 more variable: census_age_groups_6 <chr>
```
