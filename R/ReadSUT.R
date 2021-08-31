#' Read in Supply and Use Tables
#'
#' Read and process the Supply and Use Tables (SUTs) for the UK economy which cover the period 1997-2018.
#'
#' Supply and Use Tables are published by the Office for National Statistics. The tables which this
#' function is designed to use can be found
#' \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{here}.
#'
#' @param year Integer. Select the year (from 1997 to 2018) of data to use in the analysis.
#' @param fte Logical. If TRUE (default) use full-time equivalent (FTE) employment, if FALSE
#'            use total employees.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#'
#'
#' @export
ReadSUT <- function(year = 2018,
                    fte = TRUE,
                    FAI = FALSE) {

  y <- copy(year)


if (FAI == FALSE) {

### Read Supply Table (used to convert basic prices to purchaser prices) ######

supply <- tobalciomodel::iotable_ons[year == y,c(1:2,118:122)]

setDT(supply)

supply[, tax_prop := round(taxes/output_pp,3)]
supply[, scale_bp_to_pp := output_pp/output_bp]
supply[, scale_pp_to_bp := output_bp/output_pp]

### Read the Use table - product by product

iotable <- tobalciomodel::iotable_ons[year == y,3:107]

setnames(iotable, old = names(iotable), new = c(paste0("sec",c(1:105))) )

iotable <- as.matrix(iotable)

### GVA figures and employment by sector, create a table of technical coefficients
### (input as proportion of total output)

### coefficients

gva.total <- as.vector(as.matrix(tobalciomodel::iotable_ons[year == y,"gva.total"]))
gva.taxes <- as.vector(as.matrix(tobalciomodel::iotable_ons[year == y,"gva.taxes"]))
gva.gos   <- as.vector(as.matrix(tobalciomodel::iotable_ons[year == y,"gva.gos"]))
gva.wages <- as.vector(as.matrix(tobalciomodel::iotable_ons[year == y,"gva.wages"]))
total.output <- as.vector(as.matrix(tobalciomodel::iotable_ons[year == y,"total.output"]))

if (fte == TRUE) {
  employment <- tobalciomodel::lfs_empl_cpa[year == y, "tot_fte"]
} else {
  employment <- tobalciomodel::lfs_empl_cpa[year == y, "tot_emp"]
}
  employment <- as.vector(as.matrix(employment))


gva <- data.table(supply$CPA_code, supply$Product,
                  total.output,
                  gva.total/total.output,
                  gva.taxes/total.output,
                  gva.gos/total.output,
                  gva.wages/total.output,
                  employment,
                  employment/total.output)

setnames(gva, names(gva), c("CPA_code","Product","output",
                            "gva_coef","tax_coef","gos_coef","coe_coef","employment","empl_coef"))


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

  gva <- data.table(tobalciomodel::iotable_fai$IOC,
                    tobalciomodel::iotable_fai$Sector,
                    total.output,
                    gva.total/total.output,
                    gva.taxes/total.output,
                    gva.gos/total.output,
                    gva.wages/total.output,
                    employment,
                    employment/total.output)

  setnames(gva, names(gva), c("IOC","Sector","output",
                              "gva_coef","tax_coef","gos_coef","coe_coef","employment","empl_coef"))

  return(list(iotable = iotable,
              coefs = gva))

} # close loop for FAI = TRUE

}
