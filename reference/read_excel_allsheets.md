# Read multiple excel sheets at once

Read in multiple excel sheets from a workbook & turn them into one
tibble or a series of data frames each named after the sheet name. Each
excel sheet should have matching column names to ensure correct row
binding occurs.

## Usage

``` r
read_excel_allsheets(filename, single_frame = TRUE)
```

## Arguments

- filename:

  The file path to the .xlsx from which you want to pull all sheets

- single_frame:

  DEFAULT = TRUE; If TRUE, will combine all sheets from an excel
  workbook into a single data frame with a variable called "sheet_name"
  having the values of the different sheet names. If FALSE, will read in
  all sheets from an excel workbook but will create a separate data
  frame for each sheet (each data frame will be named after what the
  sheet was called)

## Examples
