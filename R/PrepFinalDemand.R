#' Setup Final Demand Vector
#'
#' Input changes in household and government expenditures, specify assumptions
#' about the distributions of spending and the household saving rate, and produce
#' the resulting vector of changes in final demand to model.
#'
#' @param hhold_exp Numeric. Change in household consumption measured in basic prices.
#' @param govt_exp Numeric. Change in government spending.
#' @param hhold_saving Numeric. Assumed household savings rate.
#' @param hhold_vector Character. How household spending is redistributed.
#' @param govt_vector Character. How government spending is redistributed.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ## construct a final demand vector for a fall in household spending in basic
#' ## prices of £20m and an increase in government spending of £10m.
#'
#' ## Assume households and governments distribute spending pro-rata as per
#' ## household final consumption expenditure and central government spending
#' ## distributions measured in 2018.
#'
#' ## Households assumed to save 10% of freed-up spending.
#'
#' finaldemand <- tobalciomodel::PrepFinalDemand(hhold_exp = -20,
#' govt_exp = 10,
#' hhold_saving = 0.1,
#' hhold_vector = "hhfce_noalctob",
#' govt_vector = "central")
#'
#' }
PrepFinalDemand <- function(hhold_exp,
                            govt_exp,
                            hhold_saving = 0.1,
                            hhold_vector = "hhfce_noalctob",
                            govt_vector = "central") {

  ### Distribute changes in household spending

  final_demand_hhold <- tobalciomodel::ReallocateHhold(expenditure = hhold_exp,
                                                       saving_rate = hhold_saving,
                                                       vector = hhold_vector,
                                                       vectors_data = tobalciomodel::vectors_hhold,
                                                       mapping = tobalciomodel::coicop_cpa_mapping)

  ### Distribute changes in government spending
  final_demand_govt  <- tobalciomodel::ReallocateGovt(expenditure = govt_exp,
                                                      vector = govt_vector,
                                                      vectors_data = tobalciomodel::vectors_govt)


  ### Merge and sum up the household and government vectors to get the
  ### overall change in final demand vector.

  final_demand <- merge.data.table(final_demand_hhold,
                                   final_demand_govt,
                                   by = c("CPA_code","Product"))

  final_demand[, final_demand := hhold_exp + govt_exp]

  return(final_demand)
}
