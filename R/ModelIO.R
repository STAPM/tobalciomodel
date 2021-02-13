#' Wrapper Function to Produce a Full IO Analysis
#'
#' Run all functions to model a policy scenario and produce output of an IO analysis, for a given
#' pre-loaded scenario option. Not that only one of `pref_changes` OR `tax_policy` should be specified
#' as TRUE for a given analysis run.
#'
#' @param select_year year of price, consumption, and employment data to use.
#' @param ons_table_year if using the ONS IO tables, select which years table to use
#' @param FAI logical. TRUE if using the Fraser of Allender Institute alcohol-disaggregated IO table. If FALSE, uses the ONS tables.
#' @param pref_changes logical. If TRUE, model a change in consumer preferences.
#' @param tax_policy logical. If TRUE, model a change in alcohol duties.
#' @param reallocate logical. If TRUE apply the reallocation functions to the initially calculated final demand vector.
#' @param alcohol_data MESAS aggregate alcohol data.
#' @param elasticities_data alcohol price elasticities.
#' @param duties_data alcohol duties as of January 2021.
#' @param off_trade_change vector of length 5, proportionate change in consumption of off-trade beer, cider, wine, spirits, and RTDs due to preference changes.
#' @param on_trade_change vector of length 5, proportionate change in consumption of on-trade beer, cider, wine, spirits, and RTDs due to preference changes.
#' @export
ModelIO <- function(
select_year       = 2019,
ons_table_year    = 2019,
FAI               = TRUE,
pref_changes      = FALSE,
tax_policy        = TRUE,
reallocate        = TRUE,
alcohol_data      = tobalciomodel::data_mesas_englandwales,
elasticities_data = tobalciomodel::elasticities,
duties_data       = tobalciomodel::data_alcohol_duty,
off_trade_change  = c(0,0,0,0,0),
on_trade_change   = c(0,0,0,0,0),
beer_duty         = c(1,1,1),
cider_duty        = c(0,0,0,0,0),
wine_duty         = c(0,0,0,0,0,0,0),
spirit_duty       = c(0),
rtd_duty          = c(0)
){

##### STEP 1 - OBTAIN CHANGES IN FINAL DEMAND
if (tax_policy == TRUE) {
  data <- tobalciomodel::TaxPol(alcohol_data = alcohol_data,
                                elasticities_data = elasticities_data,
                                duties_data = duties_data,
                                beer_duty = beer_duty,
                                cider_duty = cider_duty,
                                wine_duty = wine_duty,
                                spirit_duty = spirit_duty,
                                rtd_duty = rtd_duty,
                                select_year = select_year)
}
if (pref_changes == TRUE) {
  data <- tobalciomodel::PrefChanges(alcohol_data = alcohol_data,
                                     off_trade_change = off_trade_change,
                                     on_trade_change = on_trade_change,
                                     select_year = select_year)
}
##### STEP 2 - CLEAN FINAL DEMANDS INTO AN APPROPRIATE VECTOR

# clean to make a 3-vector of final demand changes
cleaned <- tobalciomodel::CleanFinalDemand(data = data)

# construct a full vector of changes in final demand
final_demand_vec  <- tobalciomodel::FinalDemandVec(data = cleaned,
                                                   FAI = FAI)
if (reallocate == FALSE) {
  demand_vec <- final_demand_vec
}

##### STEP 3 - REALLOCATION OF DEMAND IF SPECIFIED
if (reallocate == TRUE & tax_policy == TRUE) {
  reallocatedGovt <- ReallocateGovt(data = data,
                                    final_demand = final_demand_vec,
                                    FAI = FAI,
                                    select_year = select_year)
  demand_vec <- reallocatedGovt$final_demand
}
if (reallocate == TRUE & pref_changes == TRUE) {
  reallocatedHHold <- ReallocateHHold(final_demand = final_demand_vec,
                                      FAI = FAI,
                                      select_year = select_year,
                                      prorata = TRUE)
  demand_vec <- reallocatedHHold
}
##### STEP 4 - Extract Leontief Matrices and Multipliers

leontief <- tobalciomodel::LeontiefCalc(FAI = FAI,
                                        select_year = ons_table_year)

##### STEP 5 - Produce Impact Analysis

# calculate economic impacts
results <- ImpactCalc(demand.change = demand_vec,
                      multipliers = leontief,
                      select_year = select_year,
                      FAI = FAI,
                      export = FALSE,
                      path = path,
                      name = path)

return(results)

}
