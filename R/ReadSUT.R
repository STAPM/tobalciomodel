#' Read in Supply and Use Tables
#'
#' Read and process the Supply and Use Tables (SUTs) for the UK economy which cover the period 1997-2018.
#'
#' Supply and Use Tables are published by the Office for National Statistics. The tables which this
#' function is designed to use can be found
#' \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{here}.
#'
#' @param path Character. File path to the folder containing the ONS SUTs excel workbook.
#' @param year Integer. Select the year (from 1997 to 2018) of data to use in the analysis.
#' @param fte Logical. If TRUE (default) use full-time equivalent (FTE) employment, if FALSE
#'            use total employees.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#'
#'
#' @export
ReadSUT <- function(path,
                    year = 2018,
                    fte = TRUE,
                    FAI = FALSE) {

  y <- copy(year)


if (FAI == FALSE) {

### Read Supply Table (used to convert basic prices to purchaser prices) ######

supply <- readxl::read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                             sheet = paste0("Table 1 - Supply ",y),
                             range = "A3:K108",
                             col_names = TRUE)

setDT(supply)

supply <- supply[,-c(4:7)]

setnames(supply,
         names(supply),
         c("code","Product","output_bp","imports","margins","taxes","output_pp"))

supply[, tax_prop := round(taxes/output_pp,3)]
supply[, scale_bp_to_pp := output_pp/output_bp]
supply[, scale_pp_to_bp := output_bp/output_pp]

### Read the Use table - product by product

iotable <- read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                      sheet = paste0("Table 2 - Int Con ",y),
                      range = "C5:DC110")
iotable <- as.matrix(iotable)

### GVA figures and employment by sector, create a table of technical coefficients
### (input as proportion of total output)

gva.total <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                            sheet = paste0("Table 2 - Int Con ",y),
                                            range = "C115:DC115",
                                            col_names = FALSE)))
gva.taxes <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                            sheet = paste0("Table 2 - Int Con ",y),
                                            range = "C112:DC112",
                                            col_names = FALSE)))
gva.gos   <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                            sheet = paste0("Table 2 - Int Con ",y),
                                            range = "C114:DC114",
                                            col_names = FALSE)))
gva.wages <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                            sheet = paste0("Table 2 - Int Con ",y),
                                            range = "C113:DC113",
                                            col_names = FALSE)))

total.output <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                               sheet = paste0("Table 2 - Int Con ",y),
                                               range = "C116:DC116",
                                               col_names = FALSE) ))

if (fte == TRUE) {
  employment <- tobalciomodel::lfs_empl_cpa[year == y, "tot_fte"]
} else {
  employment <- tobalciomodel::lfs_empl_cpa[year == y, "tot_emp"]
}
  employment <- as.vector(as.matrix(employment))

gva <- matrix(c(supply$Product,
                total.output,
                employment,
                employment/total.output,
                gva.total/total.output,
                gva.taxes/total.output,
                gva.gos/total.output,
                gva.wages/total.output),
              nrow = 105,
              byrow = FALSE,
              dimnames = list(NULL,
                              c("Product","output","employment","empl_coef","gva_coef","tax_coef","gos_coef","coe_coef")))

return(list(supply = supply,
            iotable = iotable,
            coefs = gva))

} else if (FAI == TRUE) {

  ### No supply table

  ### io table

  iotable <- as.matrix(tobalciomodel::iotable_fai[,3:108])

  ### coefficients

  gva.total <- as.vector(as.matrix(tobalciomodel::iotable_fai[,"gva.total"]))
  gva.taxes <- as.vector(as.matrix(tobalciomodel::iotable_fai[,"gva.taxes"]))
  gva.gos   <- as.vector(as.matrix(tobalciomodel::iotable_fai[,"gva.gos"]))
  gva.wages <- as.vector(as.matrix(tobalciomodel::iotable_fai[,"gva.wages"]))
  total.output <- as.vector(as.matrix(tobalciomodel::iotable_fai[,"total.output"]))

  if (fte == TRUE) {
    employment <- tobalciomodel::lfs_empl_fai[year == y, "tot_fte"]
  } else {
    employment <- tobalciomodel::lfs_empl_fai[year == y, "tot_emp"]
  }
  employment <- as.vector(as.matrix(employment))

  gva <- matrix(c(tobalciomodel::iotable_fai$IOC,
                  tobalciomodel::iotable_fai$Sector,
                  total.output,
                  employment,
                  employment/total.output,
                  gva.total/total.output,
                  gva.taxes/total.output,
                  gva.gos/total.output,
                  gva.wages/total.output),
                nrow = 106,
                byrow = FALSE,
                dimnames = list(NULL,
                                c("IOC","Sector","output","employment","empl_coef","gva_coef","tax_coef","gos_coef","coe_coef")))

  return(list(iotable = iotable,
              coefs = gva))

}

}
