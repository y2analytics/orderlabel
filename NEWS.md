# orderlabel 0.3.3
## New features 
* `add_ages()` - New argument: "year_of_survey". DEFAULT = Whatever the current year is. Used to calculate respondent ages. If you are using old survey data from a previous year, you can input the year here as a numeric value (e.g. 1994)


# orderlabel 0.3.1
## Improvements
*`add_ages()` - census_age_groups now an ordered factor variable instead of a character variable. Function also now creates "census_age_groups_6" (same as original census_age_groups, but with the 18-34 group split into 18-24 & 25-34)
## Bugs
*`order_label()` - Problem fixed where order_label() would sometimes break when freqs was grouped by user-created variables


# orderlabel 0.3.0
## Possible breaking changes
*`order_label()` - TLDR: Order of rows in output now arranged by factor level, so order_label now works with updated versions of htmltools and mschart. Details are...
*This update fixes the ordering problems introduced in htmltools versions > 3.6.0 and mschart versions > 2.5.0. To do this, the frequency output from order_label may look slightly different in some cases. The label/group_var are still factored in the same order as before, but the physical arrangement is different in some cases. Because mschart now pulls from the "physical order" the tables are arranged in, and not the "factor order", the frequency table will look different in some cases to match mschart, but the charts will all be ordered correctly, regardless of if they are run in mschart or ggplot2. 


# orderlabel 0.2.3
## Breaking changes
* `topline()` function name changed to `order_topline()` because the y2_municipal package has a function called topline now.  Shouldn't be a problem because I bet no one used this function anyway. The developers didn't even use it ;)

## Bugs
* New error kept occuring when trying to use the `topbox` argument in `order_label`, but that error is now fixed!


# orderlabel 0.2.2
## Bugs
* All examples now working
* Fixed strange error showing up in some cases: "length of condition > 1"
* Eliminated notes about missing global variables


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
