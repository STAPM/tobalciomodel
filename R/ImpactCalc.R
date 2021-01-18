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
demand.change = fdemand_fai_govt$final_demand,
multipliers = leontief_fai,
select_year = 2010,
FAI = TRUE,
export = FALSE,
path = "output/",
name = "example"
) {

# extract multipliers from the list object
mult <- multipliers$multipliers

output.effects.0 <- round(sum(demand.change*mult[,"output.multipliers.type0"]))
output.effects.1 <- round(sum(demand.change*mult[,"output.multipliers.type1"]))
output.effects.2 <- round(sum(demand.change*mult[,"output.multipliers.type2"]))

gva.effects.0 <- round(sum(demand.change*mult[,"gva.multipliers.type0"]))
gva.effects.1 <- round(sum(demand.change*mult[,"gva.multipliers.type1"]))
gva.effects.2 <- round(sum(demand.change*mult[,"gva.multipliers.type2"]))

coe.effects.0 <- round(sum(demand.change*mult[,"coe.multipliers.type0"]))
coe.effects.1 <- round(sum(demand.change*mult[,"coe.multipliers.type1"]))
coe.effects.2 <- round(sum(demand.change*mult[,"coe.multipliers.type2"]))

tax.effects.0 <- round(sum(demand.change*mult[,"tax.multipliers.type0"]))
tax.effects.1 <- round(sum(demand.change*mult[,"tax.multipliers.type1"]))
tax.effects.2 <- round(sum(demand.change*mult[,"tax.multipliers.type2"]))

gos.effects.0 <- round(sum(demand.change*mult[,"gos.multipliers.type0"]))
gos.effects.1 <- round(sum(demand.change*mult[,"gos.multipliers.type1"]))
gos.effects.2 <- round(sum(demand.change*mult[,"gos.multipliers.type2"]))

# combine into a matrix - Type I and II effects for output, GVA, and employment

effects <- matrix(c(output.effects.0,
                    output.effects.1 - output.effects.0,
                    output.effects.2 - output.effects.1,
                    output.effects.2,
                    gva.effects.0,
                    gva.effects.1 - gva.effects.0,
                    gva.effects.2 - gva.effects.1,
                    gva.effects.2,
                    coe.effects.0,
                    coe.effects.1 - coe.effects.0,
                    coe.effects.2 - coe.effects.1,
                    coe.effects.2,
                    tax.effects.0,
                    tax.effects.1 - tax.effects.0,
                    tax.effects.2 - tax.effects.1,
                    tax.effects.2,
                    gos.effects.0,
                    gos.effects.1 - gos.effects.0,
                    gos.effects.2 - gos.effects.1,
                    gos.effects.2),
                  byrow=FALSE,
                  ncol = 5,
                  dimnames = list(c("Direct Effect","Indirect Effect","Induced Effect","Total Effect"),
                                  c("Output (£mn)","Total GVA (£mn)","Hhold GVA (£mn)","Govt GVA (£mn)","Firm GVA (£mn)")))

if (FAI == TRUE) {
  #### read in and clean the employment data
  empl <- tobalciomodel::employment[,c(1:11)]
  setDT(empl)
  employment <- melt.data.table(empl,id.vars = "sector",value.name = "employment")
  # filter on year
  employment <- employment[variable == paste0("fte_",select_year),c("sector","employment")]
  #### this produces a 106 vector of employment by sector in the correct year

  #### read in the compensation of employees
  coe <- tobalciomodel::data_iotable_fai[,"hhold.output"]

  #### coe per employee
  coe_per_employee = coe/employment$employment

  #### change in coe/coe per employee to get change in employment
  emp.effects.0 <- round(sum(demand.change*mult[,"coe.multipliers.type0"]/coe_per_employee))
  emp.effects.1 <- round(sum(demand.change*mult[,"coe.multipliers.type1"]/coe_per_employee))
  emp.effects.2 <- round(sum(demand.change*mult[,"coe.multipliers.type2"]/coe_per_employee))

  ### update the "effects" table

  effects <- matrix(c(output.effects.0,
                      output.effects.1 - output.effects.0,
                      output.effects.2 - output.effects.1,
                      output.effects.2,
                      gva.effects.0,
                      gva.effects.1 - gva.effects.0,
                      gva.effects.2 - gva.effects.1,
                      gva.effects.2,
                      coe.effects.0,
                      coe.effects.1 - coe.effects.0,
                      coe.effects.2 - coe.effects.1,
                      coe.effects.2,
                      tax.effects.0,
                      tax.effects.1 - tax.effects.0,
                      tax.effects.2 - tax.effects.1,
                      tax.effects.2,
                      gos.effects.0,
                      gos.effects.1 - gos.effects.0,
                      gos.effects.2 - gos.effects.1,
                      gos.effects.2,
                      emp.effects.0,
                      emp.effects.1 - emp.effects.0,
                      emp.effects.2 - emp.effects.1,
                      emp.effects.2),
                    byrow=FALSE,
                    ncol = 6,
                    dimnames = list(c("Direct Effect","Indirect Effect","Induced Effect","Total Effect"),
                                    c("Output (£mn)","Total GVA (£mn)","Hhold GVA (£mn)","Govt GVA (£mn)","Firm GVA (£mn)","Employment")))

}

if (export == TRUE) {
  write.csv(effects,paste0(path,name,".csv"))
}

  return(effects)
}
