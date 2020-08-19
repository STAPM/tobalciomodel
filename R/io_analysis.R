#' Wrapper Function to Produce a Full IO Analysis
#'
#' Run all functions to model a policy scenario and produce output of an IO analysis, for a given
#' pre-loaded scenario option.
#'
#' @param scenario the row number of the data frame `tobalciomodel::scenarios` corresponding to the chosen scenario to model.
#'
#' @export
io.analysis <- function(scenario = NULL) {
### select a pre-loaded scenario
scenario <- select_scenario(scenario)

### Select data and parameters
inputs <- final_demand_inputs(yr =         scenario[["year"]],
                              scotland =   scenario[["scotland"]],
                              elasticity = scenario[["elasticity"]])

### Simulate an alcohol policy
policy_sim <- simulate_alcohol_policy(data = inputs,
                                      alc.policy   = scenario[["alc.policy"]],
                                      on.trade.ch  = scenario[["alc.on.ex"]],
                                      off.trade.ch = scenario[["alc.off.ex"]],
                                      prob = FALSE)

### Simulate a tobacco policy


### Sum joint tobacco/alcohol effects



### Extract the multipliers
multipliers <- multipliers(yr =    scenario[["year"]],
                           empl =  scenario[["emp.measure"]])

### Output effects

output <- impact_calculate(demand.change = policy_sim,
                           multipliers = multipliers)

return(output)
}
