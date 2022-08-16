#' Write Results
#'
#' Pass a set of results from InputOutput to this function to write out a comparison spreadsheet of results
#' based on the excel template iomodel_comparison_sheet.xlsx and save to a specified output folder.
#'
#' @param path Character. File path to where the outputs of InputOutput are stored.
#' @param template Character. File path to where the results spreadsheet is stored.
#' @param label Character. Label for the batch of STAPM model runs.
#' @param out_label Character vector. Vector of IO model results RDS files suffixes (after "iomodel_output").
#' @param col_label Character vector. Must be the same length as out_label. Label for each IO model
#' to write to the columns of the spreadsheet.
#' @param write_output Character. File path to the folder in which to save the spreadsheet.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
WriteResultsTab <- function(path,
                            template,
                            label,
                            out_label,
                            col_label,
                            write_output){

  # Load the spreadsheet
  wb <- openxlsx::loadWorkbook(paste0(here::here(template), "/iomodel_comparison_sheet.xlsx" ))

  for(i in 1:length(out_label)){

    col <- i + 1

    openxlsx::writeData(wb, sheet = "T1 - IO Model Assumptions", x = col_label[i] , startCol = col, startRow = 1)

    ######################
    ## Read in data

    results <- readRDS(paste0(here::here(path), "/iomodel_output", out_label[i], ".rds"))

    ######################
    ## Model Assumptions

    data <- as.vector(as.matrix(results$assumptions))

    openxlsx::writeData(wb, sheet = "T1 - IO Model Assumptions", x = data , startCol = col, startRow = 2)

    ######################
    ## Aggregate Impacts

    data <- copy(results$aggregate)

    openxlsx::writeData(wb, sheet = "T2 - Aggregate Economic Impacts", x = as.vector(as.matrix(data[,"output"]))     , startCol = col, startRow = 3)
    openxlsx::writeData(wb, sheet = "T2 - Aggregate Economic Impacts", x = as.vector(as.matrix(data[,"gva"]))        , startCol = col, startRow = 9)
    openxlsx::writeData(wb, sheet = "T2 - Aggregate Economic Impacts", x = as.vector(as.matrix(data[,"employment"])) , startCol = col, startRow = 15)
    openxlsx::writeData(wb, sheet = "T2 - Aggregate Economic Impacts", x = as.vector(as.matrix(data[,"net_earn"]))   , startCol = col, startRow = 21)
    openxlsx::writeData(wb, sheet = "T2 - Aggregate Economic Impacts", x = as.vector(as.matrix(data[,"inc_tax"]))    , startCol = col, startRow = 27)

  }

  # Save the workbook
  saveWorkbook(wb, paste0(here::here(write_output), "/economic_impact_summary_", label, ".xlsx"), overwrite = T)

}
