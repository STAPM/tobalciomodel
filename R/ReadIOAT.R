#' Read in Input-Output Tables
#'
#' Read and process the input-output analytical table (IOAT) for the UK economy.
#'
#' Supply and Use Tables are published by the Office for National Statistics. The tables which this
#' function is designed to use are downloaded from
#' \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{this ONS page}.
#'
#' @param country Character. Country of analysis. Options are c("UK","scotland","nireland").
#' @param year_ioat Numeric. Year of the input-output analytical tables used.
#' @param fte Logical. If TRUE (default) use full-time equivalent (FTE) employment, if FALSE
#'            use total employees.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#'
#'
#' @export
ReadIOAT <- function(country,
                     year_ioat = 2010,
                     fte = TRUE,
                     FAI = TRUE) {

  y <- copy(year_ioat)

  #################################
  ### Grab the selected IO tables

  if (country == "UK") {
    data <- tobalciomodel::iotable_fai

  } else if (country == "scotland") {

    data <- tobalciomodel::iotable_scot

  } else if (country == "nireland") {

    data <- tobalciomodel::iotable_nire
  }

  ### flow table (keep only columns of data with "sec" in the name)

  flowtable <- data[, grepl( "sec" , names( data  ) ), with = FALSE]
  flowtable <- as.matrix(flowtable)

  ### coefficients

  gva.total <- as.vector(as.matrix(data[,"gva.total"]))
  gva.taxes <- as.vector(as.matrix(data[,"gva.taxes"]))
  gva.gos   <- as.vector(as.matrix(data[,"gva.gos"]))
  gva.wages <- as.vector(as.matrix(data[,"gva.wages"]))
  total.output <- as.vector(as.matrix(data[,"total.output"]))

  ### Match in employment data

  if (fte == TRUE) {
    employment <- tobalciomodel::lfs_empl_fai[year == year_ioat, "tot_fte"]

  } else {

    employment <- tobalciomodel::lfs_empl_fai[year == year_ioat, "tot_emp"]
  }

  employment <- as.vector(as.matrix(employment))

  gva <- data.table(data$IOC,
                    data$Sector,
                    total.output,
                    gva.total/total.output,
                    gva.taxes/total.output,
                    gva.gos/total.output,
                    gva.wages/total.output,
                    employment/total.output)

  setnames(gva, names(gva), c("IOC","Sector","output",
                              "gva_coef","tax_coef","gos_coef","coe_coef","empl_coef"))

  return(list(iotable = flowtable,
              coefs = gva))

}
