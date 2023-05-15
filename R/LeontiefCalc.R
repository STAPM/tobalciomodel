#' Calculate L Matrices and Multipliers
#'
#' Take the processed Input-Output and Supply and Use Tables and derive the Leontief inverse matrix and
#' multipliers required for the economic impact analysis. The function produces multipliers and Leontief
#' matrices for type 0 (direct), type 1 (indirect), and type 2 (induced) effects on output, GVA, and employment.
#'
#' @param list List. The output of the ReadIOAT() function containing the IO table and
#' technical coefficients.
#' @param country Character. Country of analysis. Options are c("UK","scotland","nireland").
#' @param year_ioat Numeric. Year of the input-output analytical tables used.
#'
#' @return Data
#' @export
#'
#'
#' @examples
#'
#' \dontrun{
#'
#'}
LeontiefCalc <- function(list,
                         country,
                         year_ioat) {

  ## extract objects from list
  iotable <- list$iotable
  coefs   <- list$coefs

  total.output <- as.vector(as.matrix(coefs[,"output"]))

  ########################################
  ## Leontief Type 0 (identity matrix ) ##

  L0 <- diag(length(total.output))

  ##############################
  ### Leontief Type 1 ##########

  ## A matrix

  A <- iotable %*% ((total.output )^-1 * diag(length(total.output)))

  ## Leontief Inverse L = (I - A)^-1

  L1 <- solve(diag(length(total.output)) - A)

  ##############################
  ### Leontief Type 2 ##########

  ## Internalise househholds within the IO table

  if (country == "UK") {
  data <- tobalciomodel::iotable_fai
  totinc <- as.numeric(tobalciomodel::gdhi[country == "United Kingdom" & year == year_ioat, "gdhi"])

  } else if (country == "Scotland") {

  data <- tobalciomodel::iotable_scot
  totinc <- as.numeric(tobalciomodel::gdhi[country == "Scotland" & year == year_ioat, "gdhi"])

  } else if (country == "N_ireland") {

  data <- tobalciomodel::iotable_nire
  totinc <- as.numeric(tobalciomodel::gdhi[country == "Northern Ireland" & year == year_ioat, "gdhi"])
  }

  ### additional row to A matrix - household income (Comp of employees) per unit of output

  coe <- as.vector(as.matrix(data[,"hhold.output"]))
  out <- as.vector(as.matrix(data[,"total.output"]))

  row_A <- coe/out

  A2 <- rbind(A, row_A)

  ### additional column to A matrix - household expenditure per unit of total household income
  ### from all sources (COE is earned income only, so use GDHI to include all income). Divide
  ### the household demand column by the totinc scalar obtained above

  cons <- as.vector(as.matrix(data[,"hhold.demand"]))

  col_A <- cons/totinc

  ### Also need to fill in the bottom right corner by extending col_A - this is household
  ### expenditure per unit of exogenous household income (Set to 0)

  col_A <- c(col_A, 0)

  A2 <- cbind(A2, col_A)

  ## Leontief Inverse L = (I - A)^-1

  L2 <- solve(diag(length(total.output) + 1) - A2)

  ######################################
  ###### Calculate Multipliers #########

  # multipliers - multiply these by the change in demand to get the impact on the whole economy
  #       direct effects - effect of a 1:1 change in output to meet the change in demand.
  #       indirect effects - effect of change in demands for intermediate goods between sectors

  ### OUTPUT MULTIPLIERS

  output.type0 <- rep(1,length(total.output))
  output.type1 <- rep(NA,length(total.output))
  output.type2 <- rep(NA,length(total.output))

  for (i in 1:length(total.output)) {
    output.type1[i] <- sum(L1[,i])
    output.type2[i] <- sum(L2[,i])
  }
  #######################
  ### GVA MULTIPLIERS

  coef.gva <- as.vector(as.matrix(coefs[,"gva_coef"]))
  coef.tax <- as.vector(as.matrix(coefs[,"tax_coef"]))
  coef.coe <- as.vector(as.matrix(coefs[,"coe_coef"]))
  coef.gos <- as.vector(as.matrix(coefs[,"gos_coef"]))

  ### Type 0

  gva.type0 <- coef.gva
  tax.type0 <- coef.tax
  coe.type0 <- coef.coe
  gos.type0 <- coef.gos

  ### Type 1

  gva.type1 <- as.vector(coef.gva %*% L1 )
  coe.type1 <- as.vector(coef.coe %*% L1 )
  gos.type1 <- as.vector(coef.gos %*% L1 )
  tax.type1 <- as.vector(coef.tax %*% L1 )

  ### Type 2

  coef.gva2 <- c(coef.gva, 0)
  coef.tax2 <- c(coef.tax, 0)
  coef.coe2 <- c(coef.coe, 0)
  coef.gos2 <- c(coef.gos, 0)

  gva.type2 <- as.vector(coef.gva2 %*% L2 )
  coe.type2 <- as.vector(coef.coe2 %*% L2 )
  gos.type2 <- as.vector(coef.gos2 %*% L2 )
  tax.type2 <- as.vector(coef.tax2 %*% L2 )

  #################################
  ### EMPLOYMENT MULTIPLIERS

  coef.emp <- as.vector(as.matrix(coefs[,"empl_coef"]))

  emp.type0 <- coef.emp

  emp.type1 <- as.vector(coef.emp %*% L1)

  coef.emp2 <- c(coef.emp, 0)

  emp.type2 <- as.vector(coef.emp2 %*% L2)

  ####### Trim the type 2 vectors to remove the household sector (which by assumption
  ####### will always have a zero effect by the multiplier method anyway)

  gva.type2 <- gva.type2[1:length(gva.type1)]
  coe.type2 <- coe.type2[1:length(coe.type1)]
  gos.type2 <- gos.type2[1:length(gos.type1)]
  tax.type2 <- tax.type2[1:length(tax.type1)]
  emp.type2 <- emp.type2[1:length(emp.type1)]

  ## (note that the method of getting multipliers for GVA and employment is consistent with the
  ## output multiplier method i.e. summing up the columns of L1 is equivalent to multiplying L1
  ## by a vector of 1s - such a vector would be the output coefficients i.e. output/output = 1)

  multipliers <- data.table(supply,
                            output.type0, output.type1, output.type2,
                            emp.type0, emp.type1, emp.type2,
                            gva.type0, gva.type1, gva.type2,
                            coe.type0, coe.type1, coe.type2,
                            gos.type0, gos.type1, gos.type2)

  return(list(multipliers = multipliers,
              leontief0 = L0,
              leontief1 = L1,
              leontief2 = L2))


}
