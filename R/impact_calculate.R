#' Calculate Economic Impacts
#'
#' Take the vector of changes in final demand and use the relevant multipliers to calculate the
#' output, employment, and gross value added impacts of the policy.
#'
#' @param demand.change list object created by `simulate_alcohol_policy()`
#' @param multipliers data frame of multipliers used to calculate the impacts.
#' @param export logical; if TRUE saves a csv file of results.
#' @param path filepath for exporting results.
#' @param name name for results file.
#'
#' @return A table of model outputs
#'
#' @export

impact_calculate <- function(demand.change = NULL,
                             multipliers = NULL,
                             export = FALSE,
                             path = "output/",
                             name = "example") {

  output.effects.0 <- round(sum(demand.change*multipliers[,"output.multipliers.type0"]/1000),3)
  output.effects.1 <- round(sum(demand.change*multipliers[,"output.multipliers.type1"]/1000),3)
  output.effects.2 <- round(sum(demand.change*multipliers[,"output.multipliers.type2"]/1000),3)

  empl.effects.0 <- round(sum(demand.change*multipliers[,"emp.multipliers.type0"]/1000))
  empl.effects.1 <- round(sum(demand.change*multipliers[,"emp.multipliers.type1"]/1000))
  empl.effects.2 <- round(sum(demand.change*multipliers[,"emp.multipliers.type2"]/1000))

  gva.effects.0 <- round(sum(demand.change*multipliers[,"gva.multipliers.type0"]/1000),3)
  gva.effects.1 <- round(sum(demand.change*multipliers[,"gva.multipliers.type1"]/1000),3)
  gva.effects.2 <- round(sum(demand.change*multipliers[,"gva.multipliers.type2"]/1000),3)


  # combine into a matrix - Type I and II effects for output, GVA, and employment

  output <- matrix(c(output.effects.0,output.effects.1,output.effects.2,
                     gva.effects.0,gva.effects.1,gva.effects.2,
                     empl.effects.0,empl.effects.1,empl.effects.2),
                   byrow=FALSE,
                   ncol = 3,
                   dimnames = list(c("Type 0","Type 1","Type 2"),
                                   c("Output (£bn)","GVA (£bn)","Employment")))

  # combine into a matrix - Type I and II effects for output, GVA, and employment

  effects <- matrix(c(output.effects.0,
                      output.effects.1 - output.effects.0,
                      output.effects.2 - output.effects.1,
                      gva.effects.0,
                      gva.effects.1 - gva.effects.0,
                      gva.effects.2 - gva.effects.1,
                      empl.effects.0,
                      empl.effects.1 - empl.effects.0,
                      empl.effects.2 - empl.effects.1),
                   byrow=FALSE,
                   ncol = 3,
                   dimnames = list(c("Direct Effect","Indirect Effect","Induced Effect"),
                                   c("Output (£bn)","GVA (£bn)","Employment")))

  print(effects)

if (export == TRUE) {
  write.csv(effects,paste0(path,name,".csv"))
}
  return(effects)
}
