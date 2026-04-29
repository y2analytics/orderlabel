# Auto remove question preambles for multiple choice/matrix questions

Takes a dataframe (frequencies) and removes the usual question preambles
so that you are left with only the labels you care about. For example,
"What is your favorite color? - Blue" becomes "Blue"

## Usage

``` r
preamble_rm(dataset, var = prompt, before_symbol = "- ")
```

## Arguments

- dataset:

  The name of the data frame for the function to modify, usually piped
  in after running freqs. You almost never need any arguments in this
  function.

- var:

  DEFAULT = prompt; If you use the prompt = TRUE argument in freqs(),
  this default should be perfect for you.

- before_symbol:

  DEFAULT = "- "; preamble_rm will remove everything before this symbol
  or string of symbols

## Examples

``` r
frequencies <- tibble::tibble(
  label = c('Selected','Selected','Selected','Selected','Selected'),
  result = c(.25, .15, .20, .10, .30),
  prompt = c(
  'What is your favorite color? - Blue',
  'What is your favorite color? - Green',
  'What is your favorite color? - Yellow',
  'What is your favorite color? - Purple',
  'What is your favorite color? - Orange'
  ),
  prompt2 = c(
  'What is your favorite color? ... Blue',
  'What is your favorite color? ... Green',
  'What is your favorite color? ... Yellow',
  'What is your favorite color? ... Purple',
  'What is your favorite color? ... Orange'
  )
)

preamble_rm(frequencies)
#> # A tibble: 5 × 4
#>   label    result prompt prompt2                                
#>   <chr>     <dbl> <chr>  <chr>                                  
#> 1 Selected   0.25 Blue   What is your favorite color? ... Blue  
#> 2 Selected   0.15 Green  What is your favorite color? ... Green 
#> 3 Selected   0.2  Yellow What is your favorite color? ... Yellow
#> 4 Selected   0.1  Purple What is your favorite color? ... Purple
#> 5 Selected   0.3  Orange What is your favorite color? ... Orange
preamble_rm(frequencies, var = prompt2, before_symbol = '\\. ')
#> # A tibble: 5 × 4
#>   label    result prompt                                prompt2
#>   <chr>     <dbl> <chr>                                 <chr>  
#> 1 Selected   0.25 What is your favorite color? - Blue   Blue   
#> 2 Selected   0.15 What is your favorite color? - Green  Green  
#> 3 Selected   0.2  What is your favorite color? - Yellow Yellow 
#> 4 Selected   0.1  What is your favorite color? - Purple Purple 
#> 5 Selected   0.3  What is your favorite color? - Orange Orange 
frequencies |> preamble_rm()
#> # A tibble: 5 × 4
#>   label    result prompt prompt2                                
#>   <chr>     <dbl> <chr>  <chr>                                  
#> 1 Selected   0.25 Blue   What is your favorite color? ... Blue  
#> 2 Selected   0.15 Green  What is your favorite color? ... Green 
#> 3 Selected   0.2  Yellow What is your favorite color? ... Yellow
#> 4 Selected   0.1  Purple What is your favorite color? ... Purple
#> 5 Selected   0.3  Orange What is your favorite color? ... Orange
```
