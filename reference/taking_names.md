# Create a dataframe of varaible names and labels

Takes a dataset (responses) and creates a new df with two columns: 1)
the variable names of your original dataset, 2) the labels of those
variables (question wording from a sav/rds file)

## Usage

``` r
taking_names(dataset = responses)
```

## Arguments

- dataset:

  DEFAULT = responses; The name of the data frame for the function to
  use, typically responses

## Examples

``` r
names <- taking_names(mtcars)

responses <- mtcars
names <- taking_names()
#> Error in taking_names(): object 'responses' not found
```
