#' Produce Sectoral Output Multipliers
#'
#' Calculate the Leontief inverse Type I and Type II matrices from the package data and calculate
#' the sector by sector multipliers for output, GVA, and employment.
#'
#' @param yr year of employment data to use
#' @param empl measure of employment to use - "employed" for number of individuals in employment
#' "fte" for full time equivalents (default).
#'
#' @export
multipliers <- function(yr = 2010,
                        empl = "fte") {

  ## extract flow table and output/demand vectors

  flowtable <- as.matrix(tobalciomodel::iotable[,grep("^[sec]", names(tobalciomodel::iotable), value=TRUE)])
  total.output <- as.vector(as.matrix(tobalciomodel::iotable[,"total.output"]))
  hhold.demand <- as.vector(as.matrix(tobalciomodel::iotable[,"hhold.demand"]))
  hhold.output <- as.vector(as.matrix(tobalciomodel::iotable[,"hhold.output"]))

  ### Create the type 1 matrix

  ## Calculate coefficient matrix:
  A <- flowtable %*% ((total.output )^-1 * diag(length(total.output)))
  # Show A

  # Identity matrix minus A
  IminusA <- diag(length(total.output)) - A

  # Calculate the Leontief Inverse matrix
  L <- solve(IminusA)


  ## Create the type 2 matrix - endoegenous households
  # create a new flowtable - add the employee output/compensation as an extra row and household demand as an extra columns
  # the household-household transfer on the lead diagonal should be zero (households do not supply/demand from/to each other directly)

  # in calculating A, the household consumption column is a fraction of total household
  # income - total employee compensation plus total gross operating surplus

  total.hhold.income <- 801796 + 504498


  flow.table2 <- rbind(flowtable,hhold.output)        # add employment earnings as an extra row
  flow.table2 <- cbind(flow.table2,c(hhold.demand,0))  # add household spending as an extra column (0 for no inter-household purchases)

  ## Calculate coefficient matrix:
  A2 <- flow.table2 %*% (( c(total.output,total.hhold.income) )^-1 * diag(length( c(total.output,total.hhold.income)  ) ) )

  # Identity matrix minus A
  IminusA2 <- diag(length(c(total.output,sum(total.output)))) - A2

  # Calculate the Leontief Inverse matrix
  L2 <- solve(IminusA2)

  ### Calculate Sectoral Multipliers

  output.multipliers.type1 <- rep(NA,106)
  output.multipliers.type2 <- rep(NA,106)
  for (i in 1:106) {
  output.multipliers.type1[i] <- sum(L[,i])
  output.multipliers.type2[i] <- sum(L2[,i])
  }

  ### Calculate Output to GVA and employment coefficients
  ### (proportion of output comprised of each GVA measure)

  # extract employment from the employment data. IO table is in millions and employment
  # is in units, so convert employment into millions to obtain a 1:1 multiplier
  if (empl == "fte") {
  employment <- tobalciomodel::employment[,paste0("fte_",yr)]/1000000
  } else if (empl == "employment") {
  employment <- tobalciomodel::employment[,paste0("empl_",yr)]/1000000
  }

  coef.gva <- tobalciomodel::iotable$gva.total / tobalciomodel::iotable$total.output
  coef.tax <- tobalciomodel::iotable$gva.taxes / tobalciomodel::iotable$total.output
  coef.coe <- tobalciomodel::iotable$gva.wages / tobalciomodel::iotable$total.output
  coef.gos <- tobalciomodel::iotable$gva.gos   / tobalciomodel::iotable$total.output
  coef.emp <- employment   / tobalciomodel::iotable$total.output

  gva.multipliers.type1 <- coef.gva*output.multipliers.type1
  tax.multipliers.type1 <- coef.tax*output.multipliers.type1
  coe.multipliers.type1 <- coef.coe*output.multipliers.type1
  gos.multipliers.type1 <- coef.gos*output.multipliers.type1
  emp.multipliers.type1 <- coef.emp*output.multipliers.type1

  gva.multipliers.type2 <- coef.gva*output.multipliers.type2
  tax.multipliers.type2 <- coef.tax*output.multipliers.type2
  coe.multipliers.type2 <- coef.coe*output.multipliers.type2
  gos.multipliers.type2 <- coef.gos*output.multipliers.type2
  emp.multipliers.type2 <- coef.emp*output.multipliers.type2

  name <- as.character(tobalciomodel::iotable$name)
  data <- cbind(name,
                output.multipliers.type1,output.multipliers.type2,
                gva.multipliers.type1,gva.multipliers.type2,
                tax.multipliers.type1,tax.multipliers.type2,
                coe.multipliers.type1,coe.multipliers.type2,
                gos.multipliers.type1,gos.multipliers.type2,
                emp.multipliers.type1,emp.multipliers.type2)

  data <- data.frame(data)


  data$output.multipliers.type1 <- as.numeric(as.character(data$output.multipliers.type1))
  data$output.multipliers.type2 <- as.numeric(as.character(data$output.multipliers.type2))
  data$gva.multipliers.type1 <- as.numeric(as.character(data$gva.multipliers.type1))
  data$gva.multipliers.type2 <- as.numeric(as.character(data$gva.multipliers.type2))
  data$tax.multipliers.type1 <- as.numeric(as.character(data$tax.multipliers.type1))
  data$tax.multipliers.type2 <- as.numeric(as.character(data$tax.multipliers.type2))
  data$coe.multipliers.type1 <- as.numeric(as.character(data$coe.multipliers.type1))
  data$coe.multipliers.type2 <- as.numeric(as.character(data$coe.multipliers.type2))
  data$gos.multipliers.type1 <- as.numeric(as.character(data$gos.multipliers.type1))
  data$gos.multipliers.type2 <- as.numeric(as.character(data$gos.multipliers.type2))

  return(data)

}

