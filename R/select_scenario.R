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

return(list(
  year = year,
  base = base,
  emp.measure = emp.measure,
  alc.policy = alc.policy,
  tob.policy = tob.policy,
  change.on = change.on,
  change.off = change.off,
  change.tob = change.tob
))
}
