#### read_excel_allsheets ####
#' Read multiple excel sheets at once
#'
#' Read in multiple excel sheets from a workbook & turn them into one tibble or a series of data frames each named after the sheet name. Each excel sheet should have matching column names to ensure correct row binding occurs.
#' @param filename The file path to the .xlsx from which you want to pull all sheets
#' @param single_frame DEFAULT = TRUE; If T, will combine all sheets from an excel workbook into a single data frame with a variable called "sheet_name" having the values of the different sheet names. If F, will read in all sheets from an excel workbook but will create a separate data frame for each sheet (each data frame will be named after what the sheet was called)
#' @keywords excel xlsx sheet workbook
#' @export
#' @examples
#' # Create a single data frame with all sheets stacked
#' \dontrun{
#' responses <- read_excel_allsheets(
#' ~/Desktop/Example Excel File.xlsx')
#'
#' # Create a list and multiple data frames, one for each sheet in the excel file
#' responses <- read_excel_allsheets(
#' ~/Desktop/Example Excel File.xlsx', FALSE)
#'}

read_excel_allsheets <- function(filename, single_frame = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  ldf <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(ldf) <- sheets
  if(single_frame == TRUE){ # Creates a single data frame with all sheets combined together
    ldf <- ldf %>% dplyr::bind_rows(.id = 'sheet_name') %>% tibble::as_tibble()
  } else{ # Creates a separate data frame for each sheet, with each data frame being called the name of that sheet
    list2env(ldf, envir = .GlobalEnv)
  }
  return(ldf)
}

