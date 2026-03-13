# Add a "census_region" variable based on states

Takes a data frame with a state variable and uses those states to
categorize US census regions

## Usage

``` r
add_regions(dataset, state_var, new_name = census_region)
```

## Arguments

- dataset:

  The name of the data frame for the function to modify, usually piped
  into your main data frame.

- state_var:

  NO DEFAULT; The state variable on which census regions will be based.
  Can be state name or state abbreviations

- new_name:

  DEFAULT = census_region; the name for your new census region variable,
  as an object not a "string"

## Examples

``` r
responses <- tibble::tibble(
  state = c(
  'Virginia',
  'UTAH',
  'New York',
  'alaska',
  'Washington DC'
  )
)

add_regions(responses, state_var = state)
#> # A tibble: 5 × 2
#>   state         census_region
#>   <chr>         <chr>        
#> 1 Virginia      South        
#> 2 UTAH          West         
#> 3 New York      Northeast    
#> 4 alaska        West         
#> 5 Washington DC South        
```
