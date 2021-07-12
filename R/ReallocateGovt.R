#' Reallocate Government Expenditure
#'
#' A function to allocate increased government expenditure achieved through a duties policy
#' across the 105 CPA sectors.
#'
#' @param expenditure change in consumption, measured in basic prices.
#' @param vector character. The distribution of reallocation of spending to implement.
#' Valid options are the column names of the \code{vectors_hhold} data. The default is to
#' allocate spending pro-rata according to the 2018 distribution of central government spending.
#' @param vector_data data table containing the redistribution vectors.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#'
#'
#' @export
ReallocateGovt <- function(expenditure = 10,
                           vector = "central",
                           vectors_data = tobalciomodel::vectors_govt,
                           FAI = FALSE
) {

  # calculate the amount of expenditure that will be reallocated

  exp <- expenditure

  # select the chosen reallocation vector

  if (vector == "central") {
    v <- as.vector(as.matrix( vectors_data[,3] ))
  } else if (vector == "local") {
    v <- as.vector(as.matrix( vectors_data[,4] ))
  } else if (vector == "total") {
    v <- as.vector(as.matrix( vectors_data[,5] ))
  } else if (vector == "all_pubadmin") {
    v <- as.vector(as.matrix( vectors_data[,6] ))
  } else if (vector == "all_education") {
    v <- as.vector(as.matrix( vectors_data[,7] ))
  } else if (vector == "all_health") {
    v <- as.vector(as.matrix( vectors_data[,8] ))
  } else if (vector == "all_socialwork") {
    v <- as.vector(as.matrix( vectors_data[,9] ))
  } else if (vector == "all_cultural") {
    v <- as.vector(as.matrix( vectors_data[,10] ))
  }

  # redistribute the expenditure along the vector

  govt_exp <- exp*v
  govt_exp <- cbind(vectors_data[,c(1,2)],govt_exp)

  if (FAI == TRUE) {

    ## extract the CPA/IOC lookup table and merge, collapsing by FAI categories
    merge_data <- unique(tobalciomodel::sic_cpa_fai_mapping[,c("CPA_code","Product","IOC","Sector")])

    map_to_FAI <- merge(govt_exp, merge_data, by = c("CPA_code","Product"))

    FAI_data <- map_to_FAI[, .(govt_exp = sum(govt_exp)), by = c("IOC","Sector")]

    ## merge to the names of the FAI IO table to get the 3 disaggregated alcohol sectors

  sectors <- as.data.frame(tobalciomodel::iotable_fai[,"name"])
  setDT(sectors)
  setnames(sectors, names(sectors), "Sector")

  FAI_data <- merge(sectors, FAI_data, by = "Sector", all = TRUE, sort = FALSE)

  ## fill in the three alcohol categories and manufacture of tobacco with the initial changes
  ## to expenditure (these are always 0 for govt spending)

  FAI_data[61, govt_exp := 0]
  FAI_data[c(69,71), govt_exp := 0]
  FAI_data[18, govt_exp := 0]

  govt_exp <- merge(sectors, FAI_data, by = "Sector", sort = FALSE)

  }

  return(govt_exp)
}
