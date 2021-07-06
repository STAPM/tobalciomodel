#' Read IO Model Inputs from STAPM Output
#'
#' Function to read in and further process the outputs of STAPM price policy
#' modelling economic outcomes. Produces the change in demand for tobacco, on-trade
#' alcohol and off-trade alcohol measured in basic prices and also the net change
#' in government revenue.
#'
#' @param inputs Data table. Output from the `stapmr` package function `EconCalc()`
#' @param policy_effect_year Numeric. Year for which economic impacts are to be modelled.
#' @param quantity Logical. If TRUE calculate the effects of a non-price induced change in quantity consumed.
#' Changes are calculated relative to the control arm of the simulation. Defaults to FALSE.
#' @param quantity_prop Numeric vector of length 3. The proportionate exogenous change in consumption to model for
#' off-trade alcohol, on-trade alcohol, and tobacco respectively.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ### Read in the output of a price policy simulation
#'
#' ### Obtain economic outcomes
#'
#'
#' ### use the output from EconCalc as the input to the
#' ### function to clean IO model inputs.
#'
#' iomodel_inputs <- ReadInputs(econ_calcs)
#'
#' }
ReadInputs <- function(inputs,
                       policy_effect_year,
                       quantity = FALSE,
                       quantity_prop = c(-0.1,-0.1,-0.1)) {

  ## restrict to the policy effect year

  inputs <- inputs[year == policy_effect_year,]

  inputs <- inputs[,c("arm","product","year","basic_price_mean","price_mean","cons_tot","tot_tr_annual")]

  ## reshape wide

  wide <- dcast(inputs,
                product + year ~ arm,
                value.var = c("basic_price_mean","price_mean","cons_tot","tot_tr_annual"))

  ### Calculate the changes in consumer demand at basic prices and net change in govt revenue

  if (quantity == FALSE) {

  wide[, demand_change := basic_price_mean_control*(cons_tot_treatment - cons_tot_control)]

  final_demand_vec <- as.matrix(wide[,c("product","demand_change")])

  wide[, net_tax_gain := tot_tr_annual_treatment - tot_tr_annual_control]

  govt_revenue <- sum(wide[,"net_tax_gain"])

  } else if (quantity == TRUE) {

  wide[product == "alc_off", prop := quantity_prop[1]]
  wide[product == "alc_on", prop := quantity_prop[2]]
  wide[product == "tob_all", prop := quantity_prop[3]]

  wide[, demand_change := basic_price_mean_control*( (cons_tot_control*(1+prop) ) - cons_tot_control)]

  final_demand_vec <- as.matrix(wide[,c("product","demand_change")])

  wide[, net_tax_gain := (tot_tr_annual_control*(1+prop)) - tot_tr_annual_control]


  }

return(list(final_demand_vec = final_demand_vec,
            govt_revenue = govt_revenue))

}
