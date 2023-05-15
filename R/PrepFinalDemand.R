#' Setup Final Demand Vector
#'
#' Input changes in household and government expenditures, specify assumptions
#' about the distributions of spending and the household saving rate, and produce
#' the resulting vector of changes in final demand to model.
#'
#' @param hhold_exp Numeric vector of length 3. Change in household consumption measured in basic prices for off-trade alcohol,
#' on-trade alcohol, and tobacco.
#' @param govt_revenue Numeric. change in government revenues, measured in basic prices.
#' @param hhold_passthru Numeric. Assumed household rate of passthrough - the proportion of change in spending which
#' is compensated for in spending on other consumption categories. Defaults to 1 (full passthrough).
#' @param govt_passthru Numeric. Assumed government rate of passthrough - the proportion of change in revenues
#' which are matched by changes in government expenditure. Defaults to 0 (no passthrough).
#' @param hhold_reallocate Numeric (1-3). The distribution of reallocation of spending to implement from the \code{vectors_hhold} data.
#' Option 1 allocates pro-rata across all consumption categories, option 2 excludes alcohol and tobacco consumption, option 3
#' (default) further excludes health, education, rents and utilities.
#' @param govt_reallocate Numeric (1-5). The distribution of reallocation of spending to implement from the \code{vectors_govt} data.
#' Option 1 (default) allocates pro-rata according to the distribution of total government spending,
#' option 2 allocates according to central government spending only, option 3 allocates according
#' to local government only. Option 4 allocates all spending to health, and option 5 allocates all spending
#' to education.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#'
#' @return Data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
PrepFinalDemand <- function(hhold_exp,
                            govt_revenue,
                            hhold_passthru = 1,
                            govt_passthru = 0,
                            hhold_reallocate = 3,
                            govt_reallocate = 1,
                            FAI = FALSE) {

  ### Distribute changes in household spending

  final_demand_hhold <- tobalciomodel::ReallocateHhold(expenditure = hhold_exp,
                                                       hhold_passthru = hhold_passthru,
                                                       vector = hhold_reallocate,
                                                       vectors_data = tobalciomodel::vectors_hhold,
                                                       mapping = tobalciomodel::coicop_cpa_mapping,
                                                       FAI = FAI)

  ### Distribute changes in government spending
  final_demand_govt  <- tobalciomodel::ReallocateGovt(revenue = govt_revenue,
                                                      govt_passthru = govt_passthru,
                                                      vector = govt_reallocate,
                                                      vectors_data = tobalciomodel::vectors_govt,
                                                      FAI = FAI)


  ### Merge and sum up the household and government vectors to get the
  ### overall change in final demand vector.

  if (FAI == FALSE) {
  final_demand <- merge.data.table(final_demand_hhold,
                                   final_demand_govt,
                                   by = c("CPA_code","Product"), sort = FALSE)

  } else if (FAI == TRUE) {
    final_demand <- merge.data.table(final_demand_hhold,
                                     final_demand_govt,
                                     by = c("IOC","Sector"), sort = FALSE)
  }

  final_demand[, final_demand := hhold_exp + govt_exp]


  return(final_demand)
}
