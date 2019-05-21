#### read_excel_allsheets ####
#' 2+ excel sheets -> 1 data frame
#'
#' Read in multiple excel sheets in a workbook & turn them into one tibble. Each excel sheet should have matching column names and orders to ensure correct row binding occurs
#' @param filename The file path to the .xlsx you want to pull all sheets from
#' @param tibble DEFAULT = TRUE
#' @keywords
#' @export
#' @examples
#' responses <- read_excel_allsheets('~/filepath.xlsx')
#

read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x %>% dplyr::bind_rows()
}

