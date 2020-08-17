#' Calculate Economic Impacts
#'
#' Take the vector of changes in final demand and use the relevant multipliers to calculate the
#' output, employment, and gross value added impacts of the policy.
#'
#' @param demand.change list object created by `simulate_alcohol_policy()`
#'
#' @return A table of model outputs
#'
#' @export

impact_calculate <- function(demand.change = NULL,
                             multipliers = NULL) {

  output.effects.1 <- round(sum(demand.change$final.demand*multipliers[,"output.multipliers.type1"]),3)
  output.effects.2 <- round(sum(demand.change$final.demand*multipliers[,"output.multipliers.type2"]),3)

  empl.effects.1 <- round(sum(demand.change$final.demand*multipliers[,"emp.multipliers.type1"]))
  empl.effects.2 <- round(sum(demand.change$final.demand*multipliers[,"emp.multipliers.type2"]))

  gva.effects.1 <- round(sum(demand.change$final.demand*multipliers[,"gva.multipliers.type1"]),3)
  gva.effects.2 <- round(sum(demand.change$final.demand*multipliers[,"gva.multipliers.type2"]),3)


  # combine into a matrix - Type I and II effects for output, GVA, and employment

  output <- matrix(c(output.effects.1,output.effects.2,
                     gva.effects.1,gva.effects.2,
                     empl.effects.1,empl.effects.2),
                   byrow=FALSE,
                   ncol = 3,
                   dimnames = list(c("Type 1","Type 2"),
                                   c("Output","GVA","Employment")))

  return(output)
}
