#' Read IO Model Inputs from STAPM Output
#'
#' Function to read in and further process the outputs of STAPM price policy
#' modelling economic outcomes. Produces the change in demand for tobacco, on-trade
#' alcohol and off-trade alcohol measured in basic prices and also the net change
#' in government revenue.
#'
#' @param data Data table. Output from the `stapmr` package function `EconCalc()`
#' @param policy_effect_year Numeric. Year for which economic impacts are to be modelled.
#' @param n_years Numeric. Number of years starting with the policy effect year for which to extract treatment effects.
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
ReadInputs <- function(data,
                       policy_effect_year,
                       n_years = 1) {

  ## restrict to the selected years

  max_year <- policy_effect_year + n_years - 1

  data <- data[year %in% policy_effect_year:max_year,]

  data <- data[,c("arm","product","year","mean_basic_price","mean_price","tot_cons_annual","tot_tr_annual")]

  ## reshape wide

  wide <- dcast(data,
                year + product ~ arm,
                value.var = c("tot_cons_annual","mean_basic_price","mean_price","tot_tr_annual"))

  ### Calculate the changes in consumer demand at basic prices and net change in govt revenue

    wide[, demand_change := mean_basic_price_control*(tot_cons_annual_treatment - tot_cons_annual_control)]

    final_demand_vec <- as.data.table(wide[,c("year","product","demand_change")])
    final_demand_vec <- dcast(final_demand_vec, year ~ product, value.var = "demand_change")

    ## government revenues and total expenditure

    wide[, net_tax_gain := tot_tr_annual_treatment - tot_tr_annual_control]
    govt_revenue <- wide[, .(govt_revenue = sum(net_tax_gain)), by = "year"]

    wide[, tot_exp_change := (mean_basic_price_control*tot_cons_annual_treatment) - (mean_basic_price_control*tot_cons_annual_control)]
    tot_exp <- wide[, .(tot_exp_change = sum(tot_exp_change)), by = "year"]

    ## merge together

    merge <- merge(final_demand_vec, merge(tot_exp, govt_revenue, by = "year"), by = "year")

    setnames(merge,
             names(merge),
             c("year","exp_alc_off_bp","exp_alc_on_bp","exp_tob_bp","exp_total_bp","net_govt_revenue"))


return(merge)

}
