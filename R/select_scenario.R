#' Select Scenario Parameters
#'
#' Choose the scenario from the pre-loaded data frame of scenarios in `tobalciomodel::scenarios`
#' and extract the relevant parameters to put into the IO model. Output is a vector of changes in
#' demand (measured in £m) by sector.
#'
#' @param num.scenario the row number of the data frame `tobalciomodel::scenarios` corresponding to the chosen scenario to model.
#'
#' @export
select_scenario <- function(num.scenario = NULL) {

  data <- tobalciomodel::scenarios[num.scenario,]

  # extract parameters
  year <- data$year
  base <- data$base
  emp.measure <- data$emp.measure
  alc.policy <- data$alc.policy
  tob.policy <- data$tob.policy
  change.on  <- sum(c(data$on.beer,data$on.cider,data$on.wine,data$on.spirit,data$on.rtd))
  change.off <- sum(c(data$off.beer,data$off.cider,data$off.wine,data$off.spirit,data$off.rtd))
  change.tob <- sum(c(data$tob.fm,data$tob.ryo))

  ## Initialise a vector to store the changes in final demand to put into the IO model

  final.demand <- rep(0,106)

  # off-trade changes go into sector 61 - Wholesale Trade (Alcohol)

  final.demand[61] <- change.off

  # on-trade changes equally split between 69 - Accommodation (Alcohol) and 71 - Food and Beverage (Alcohol)

  split <- 0.5
  final.demand[69] <- change.on[1]*split
  final.demand[71] <- change.on[1]*(1-split)

  # tobacco changes go into sector 18 - manufacture of tobacco products

  final.demand[18] <- change.tob


  return(final.demand)
}
