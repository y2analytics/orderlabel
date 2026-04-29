# Auto change "Other please specify"s into "Other"

Takes a dataframe (frequencies) and replaces the usual variations of
"Other please specify" into Other. Also standardizes "None of the above"
and "Prefer not to say" options. Additionally, removes all extra text
after parentheses opening. Does this for the 'label', 'variable', and
'group_var' vars.

## Usage

``` r
other_rm(dataset, var = NULL, remove = FALSE)
```

## Arguments

- dataset:

  The name of the data frame for the function to modify, usually piped
  in after running freqs. You almost never need any arguments in this
  function.

- var:

  DEFAULT = NULL; If kept at NULL, it will already remove parenthetical
  text in 'label', 'variable', and 'group_var' vars. You can add an
  additional variable by specifying it here.

- remove:

  DEFAULT = FALSE; If set to TRUE, will remove categories of "Other" and
  "None of the above" from the frequencies

## Examples

``` r
frequencies <- tibble::tibble(
  label = c(
  'Brand 1',
  'Brand 2',
  'Brand 3',
  'Other (please specify)',
  'None of the above brands...'
  ),
  result = c(.25, .15, .20, .10, .30),
  other_var = c('x', 'y', 'z', 'z (test)', 'None')
)

frequencies |> other_rm(remove = TRUE)
#> # A tibble: 3 × 3
#>   label   result other_var
#>   <chr>    <dbl> <chr>    
#> 1 Brand 1   0.25 x        
#> 2 Brand 2   0.15 y        
#> 3 Brand 3   0.2  z        
frequencies |> other_rm(var = other_var)
#> # A tibble: 5 × 3
#>   label             result other_var        
#>   <chr>              <dbl> <chr>            
#> 1 Brand 1             0.25 x                
#> 2 Brand 2             0.15 y                
#> 3 Brand 3             0.2  z                
#> 4 Other               0.1  z                
#> 5 None of the above   0.3  None of the above
```
