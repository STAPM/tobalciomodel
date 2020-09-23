#' Calculate Economic Impacts
#'
#' Take the vector of changes in final demand and use the relevant multipliers to calculate the
#' output, employment, and gross value added impacts of the policy.
#'
#' @param demand.change vector of changes in final demand created by `demand_changes()`
#' @param multipliers data frame of multipliers used to calculate the impacts.
#' @param yr year of analysis.
#' @param base base year for real terms comparisons.
#' @param export logical; if TRUE saves a csv file of results.
#' @param path filepath for exporting results.
#' @param name name for results file.
#'
#' @return A table of model outputs
#'
#' @export

impact_calculate <- function(demand.change = NULL,
                             multipliers = NULL,
                             yr = NULL,
                             base = 2010,
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

  effects <- matrix(c(output.effects.0,
                      output.effects.1 - output.effects.0,
                      output.effects.2 - output.effects.1,
                      output.effects.2,
                      gva.effects.0,
                      gva.effects.1 - gva.effects.0,
                      gva.effects.2 - gva.effects.1,
                      gva.effects.2,
                      empl.effects.0,
                      empl.effects.1 - empl.effects.0,
                      empl.effects.2 - empl.effects.1,
                      empl.effects.2),
                   byrow=FALSE,
                   ncol = 3,
                   dimnames = list(c("Direct Effect","Indirect Effect","Induced Effect","Total Effect"),
                                   c("Output (£bn)","GVA (£bn)","Employment")))

  print(effects)
  if (export == TRUE) {
    write.csv(effects,paste0(path,name,".csv"))
  }

  ### compare to the overall size of the economy. First, check the real terms macro data and consumption effects
  ### data both use the same base year. If not, correct the macro data.

  if (base != 2010) {
    print(paste0("Base Year for Consumption Data is ",base,". Macro Data is in 2010 Prices - Rebasing to ",base," Prices"))

    macro_data <- tobalciomodel::macro
    macro_data$cpih_index <- 100*(macro_data$cpih_index/macro_data[macro_data$year==base,"cpih_index"])

    macro_data$real_gdp <- (100/macro_data$cpih_index)*macro_data$gdp
    macro_data$real_gva <- (100/macro_data$cpih_index)*macro_data$gva

  } else {
    macro_data <- tobalciomodel::macro
  }

  ### extract the necessary scalars, converting gdp and gva from £mn to £bn.

  tot.gdp <- macro_data[macro_data$year == yr,"real_gdp"]/1000
  tot.gva <- macro_data[macro_data$year == yr,"real_gva"]/1000
  tot.emp <- macro_data[macro_data$year == yr,"emp"]

  ### calculate results as percentages

  effects <- as.data.frame(effects)


  return(effects)
}
