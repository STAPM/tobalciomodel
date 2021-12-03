#' Reallocate Government Expenditure
#'
#' A function to allocate increased government expenditure achieved through a duties policy
#' across the 105 CPA sectors.
#'
#' @param revenue Numeric. change in government revenues, measured in basic prices.
#' @param govt_passthru Numeric. Assumed government rate of passthrough - the proportion of change in revenues
#' which are matched by changes in government expenditure. Defaults to 0 (no passthrough).
#' @param vector Numeric (1-5). The distribution of reallocation of spending to implement from the \code{vectors_govt} data.
#' Option 1 (default) allocates pro-rata according to the distribution of total government spending,
#' option 2 allocates according to central government spending only, option 3 allocates according
#' to local government only. Option 4 allocates all spending to health, and option 5 allocates all spending
#' to education.
#' @param vector_data data table containing the redistribution vectors.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ### generate a vector of government final demand changes for a fall in
#' ### revenues of £100m. With passthrough = 0, this will produce a vector
#' ### of 0s.
#'
#' govt_spend <- ReallocateGovt(revenue = -100,
#'                              govt_passthru = 0,
#'                              vector = 1,
#'                              vectors_data = tobalciomodel::vectors_govt,
#'                              FAI = FALSE)
#'
#' ### generate a vector of government final demand changes for a fall in
#' ### revenues of £100m. With passthrough = 1, with vector = 1, this will
#' ### impose a total cut in government spending of £100m, allocated
#' ### pro-rata across the distribution of total govt spending.
#'
#' govt_spend <- ReallocateGovt(revenue = -100,
#'                              govt_passthru = 1,
#'                              vector = 1,
#'                              vectors_data = tobalciomodel::vectors_govt,
#'                              FAI = FALSE)
#'
#' ### Passthrough = 0.5 - for a £100m fall in revenues, a cut in spending of $50m,
#' ### vector = 2 - spending cuts allocated across the distribution of central
#' ###              government spending
#'
#' govt_spend <- ReallocateGovt(revenue = -100,
#'                              govt_passthru = 0.5,
#'                              vector = 2,
#'                              vectors_data = tobalciomodel::vectors_govt,
#'                              FAI = FALSE)
#'
#' }
ReallocateGovt <- function(revenue,
                           govt_passthru = 0,
                           vector = 1,
                           vectors_data = tobalciomodel::vectors_govt,
                           FAI = FALSE) {

  # calculate the amount of additional or reduced government expenditure
  # to be allocated across sectors

  exp <- (govt_passthru)*sum(revenue)

  # select the chosen reallocation vector

  col <- names(vectors_data)[vector+2]
  v <- as.vector(as.matrix( vectors_data[, ..col] ))

  # redistribute the expenditure along the vector

  govt_exp <- exp*v
  govt_exp <- cbind(vectors_data[,c(1,2)],govt_exp)

  if (FAI == TRUE) {

    ## extract the CPA/IOC lookup table and merge, collapsing by FAI categories
    merge_data <- unique(tobalciomodel::sic_cpa_fai_mapping[,c("CPA_code","Product","IOC","Sector")])

    map_to_FAI <- merge(govt_exp, merge_data, by = c("CPA_code","Product"))

    FAI_data <- map_to_FAI[, .(govt_exp = sum(govt_exp)), by = c("IOC","Sector")]

    ## merge to the names of the FAI IO table to get the 3 disaggregated alcohol sectors

  sectors <- as.data.frame(tobalciomodel::iotable_fai[,c("IOC","Sector")])
  setDT(sectors)

  FAI_data <- merge(sectors, FAI_data, by = c("IOC", "Sector"), all = TRUE, sort = FALSE)

  ## fill in the three alcohol categories and manufacture of tobacco with the initial changes
  ## to expenditure (these are always 0 for govt spending)

  FAI_data[61, govt_exp := 0]
  FAI_data[c(69,71), govt_exp := 0]
  FAI_data[18, govt_exp := 0]

  govt_exp <- copy(FAI_data)
  }

  return(govt_exp)
}
