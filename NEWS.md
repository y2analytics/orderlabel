# orderlabel 0.2.1
## Breaking changes
* `order_label`
1. `label_secific` and `group_specific` are replaced with `label_first` and `group_first`. Naming conventions changed due to the additions of `label_last` and `group_last`.

## New arguments
* `order_label`
1. New arguments: `label_last` and `group_last`. You can use these arguments to choose a specific label or group level you want to show up last on the chart



# orderlabel 0.1.6

## New files
* NEWS.md
* README.md
* cran-comments.md

## Old functions
* `none_other` argument  of `order_label` now applies to group_var, not just label
* Solidified T and F to TRUE and FALSE (should help with minor bugs)
* Eliminated lots of function notes: missing "global variables" and "global functions"
* Updated examples and documentation


# orderlabel 0.1.5

## Old functions
* `other_rm` 
    - Now works on "variable" and "group_var" (in addition to "label")
    - "remove" argument added
* `read_excel_allsheets` 
    - Has option to create one or multiple data frames with var to indicate sheet names
* `order_same`
    - User may specify the grouping variable
* `taking_names`
    - Now creates a tibble
* `order_label`
    - num_fmt argument added
* General updates:
    - Reprex examples for functions
    
## New functions
- `preamble_rm` Have you ever gotten the freqs for matrix questions or multiple choice questions and you get stuck with long question wording attached to the prompt like "What is your favorite color - Blue"? And all you want is "Blue"? Well then this is the function for you! It automatically removes that pesky question wording.
- `add_regions` Creates a census_region variable based off a state variable you give it.
- `add_ages` Give it a year born variable or an age variable (these can still be in the original Qualtrics, factor labelled form), and it will create three new variables for you 1. year_born_numeric, 2. age_numeric, and 3. census_age_groups (using the census age groups we set for our municipal surveys)


# orderlabel 0.1.0

## Overview
* First official release on GitHub
