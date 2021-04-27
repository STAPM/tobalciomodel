#' Calculate Economic Impacts
#'
#' Take the vector of changes in final demand and use the relevant multipliers to calculate the
#' output, employment, and gross value added impacts of the policy.
#'
#' @param demand.change vector of changes in final demand
#' @param multipliers list object created by `LeontiefCalc()`.
#' @param select_year year of analysis.
#' @param FAI logical. TRUE if using the Fraser of Allender Institute IO table (the default). Select FALSE to use one of the ONS tables.
#' @param export logical; if TRUE saves a csv file of results.
#' @param path filepath for exporting results.
#' @param name name for results file.
#'
#' @return A table of model outputs
#'
#' @export
ImpactCalc <- function(
    demand.change ,
    multipliers
) {

  data <- cbind(multipliers,demand.change)

  ### Calculate output effects

  data[, output.effect.direct   := output.multipliers.type0*demand.change]
  data[, output.effect.indirect := output.multipliers.type1*demand.change - output.effect.direct]

  output.0 <- sum(data[,"output.effect.direct"])
  output.1 <- sum(data[,"output.effect.indirect"])

  ### Calculate GVA effects

  data[, gva.effect.direct   := gva.multipliers.type0*demand.change]
  data[, gva.effect.indirect := gva.multipliers.type1*gva.effect.direct - gva.effect.direct]

  gva.0 <- sum(data[,"gva.effect.direct"])
  gva.1 <- sum(data[,"gva.effect.indirect"])

  ### Calculate cost of employment effects

  data[, coe.effect.direct   := coe.multipliers.type0*demand.change]
  data[, coe.effect.indirect := coe.multipliers.type1*coe.effect.direct - coe.effect.direct]

  coe.0 <- sum(data[,"coe.effect.direct"])
  coe.1 <- sum(data[,"coe.effect.indirect"])

  ### collect into a matrix

  effects <- matrix(c(output.0,output.1,
                      gva.0,gva.1,
                      coe.0,coe.1),
                    nrow = 2,
                    byrow = FALSE,
                    dimnames = list(c("Direct Effect","Indirect Effect"),
                                    c("Output","GVA","CoE")))

  effects <- round(effects,3)


  return(effects)
}
