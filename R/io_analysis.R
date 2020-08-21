#' Wrapper Function to Produce a Full IO Analysis
#'
#' Run all functions to model a policy scenario and produce output of an IO analysis, for a given
#' pre-loaded scenario option.
#'
#' @param scenario the row number of the data frame `tobalciomodel::scenarios` corresponding to the chosen scenario to model.
#' @param hhold allow households to re-allocate spending
#' @param hhold.pro.rata arguments to pass to `hhold_reallocate()` - pro rata redistribute
#' @param hhold.sectors arguments to pass to `hhold_reallocate()` - specific sectors to re-allocate to
#' @param hhold.prop arguments to pass to `hhold_reallocate()` - proportions to re-allocate by sector
#' @export
io.analysis <- function(scenario.num = NULL,
                        hhold = FALSE,
                        hhold.pro.rata = TRUE,
                        hhold.sectors = NULL,
                        hhold.prop = NULL) {
### select a pre-loaded scenario
scenario <- select_scenario(scenario.num)

alcohol <- ifelse(is.null(scenario[["alc.policy"]]),FALSE,TRUE)
tobacco <- ifelse(is.null(scenario[["tob.policy"]]),FALSE,TRUE)

############## Model Alcohol

# create empty vector of final demand changes to overwrite
alc_policy_sim <- rep(0,106)

if (alcohol == TRUE) {
  ### Extract relevant data and parameters for the chosen scenario
  inputs <- final_demand_inputs_alc(yr =         scenario[["year"]],
                                    scotland =   scenario[["scotland"]],
                                    elasticity = scenario[["alc.elasticity"]])

  ### Simulate the alcohol policy
  alc_sim <- simulate_alcohol_policy(data = inputs,
                                           alc.policy   = scenario[["alc.policy"]],
                                           on.trade.ch  = scenario[["alc.on.ex"]],
                                           off.trade.ch = scenario[["alc.off.ex"]],
                                           prob = FALSE)
  alc_policy_sim <- alc_sim$final.demand
}

############## Model Tobacco
tob_policy_sim <- rep(0,106)
if (tobacco == TRUE) {
  ### Extract relevant data and parameters for the chosen scenario
  tob_inputs <- final_demand_inputs_tob(yr =         scenario[["year"]],
                                        elasticity = scenario[["tob.elasticity"]])

  ### Simulate the alcohol policy
  tob_policy_sim <- simulate_tobacco_policy(data =         tob_inputs,
                                            tob.policy   = scenario[["tob.policy"]],
                                            cigs.ch =      scenario[["tob.ex"]])
}
#### Sum up the tobacco and alcohol final demand changes

policy_sim <- alc_policy_sim + tob_policy_sim


#### reallocate household spending if specified in the function
if (hhold == TRUE) {
hhold <- hhold_reallocate(policy_sim,
                          pro.rata = hhold.pro.rata,
                          sectors = hhold.sectors,
                          prop = hhold.prop)
policy_sim <- hhold
}

### Extract the multipliers
multipliers <- multipliers(yr =    scenario[["year"]],
                           empl =  scenario[["emp.measure"]])

### Output effects

output <- impact_calculate(demand.change = policy_sim,
                           multipliers = multipliers)

return(output)
}
