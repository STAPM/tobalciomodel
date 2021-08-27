#' Calculate L Matrices and Multipliers
#'
#' Take the processed Input-Output and Supply and Use Tables and derive the Leontief inverse matrix and
#' multipliers required for the economic impact analysis.
#'
#' @param list List. The output of the ReadSUT() function containing the IO table, supply table, and
#' technical coefficients.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#'
#' @export
LeontiefCalc <- function(list,
                         FAI = FALSE) {

  ## extract objects from list
  iotable <- list$iotable
  coefs   <- list$coefs
  supply  <- list$supply

  if (FAI == TRUE) {
    supply <- tobalciomodel::iotable_fai[,c("IOC","Sector")]
    setnames
  }

  total.output <- as.numeric(coefs[,"output"])

  ## A matrix

  A <- iotable %*% ((total.output )^-1 * diag(length(total.output)))

  ## Leontief Inverse L = (I - A)^-1

  L1 <- solve(diag(length(total.output)) - A)

  ######################################
  ###### Calculate Multipliers #########

  # multipliers - multiply these by the change in demand to get the impact on the whole economy
  #       direct effects - effect of a 1:1 change in output to meet the change in demand.
  #       indirect effects - effect of change in demands for intermediate goods between sectors

  ### OUTPUT MULTIPLIERS

  output.type0 <- rep(1,length(total.output))
  output.type1 <- rep(NA,length(total.output))

  for (i in 1:length(total.output)) {
    output.type1[i] <- sum(L1[,i])
  }

  ### GVA MULTIPLIERS

  coef.gva <- as.numeric(coefs[,"gva_coef"])
  coef.tax <- as.numeric(coefs[,"tax_coef"])
  coef.coe <- as.numeric(coefs[,"coe_coef"])
  coef.gos <- as.numeric(coefs[,"gos_coef"])

  gva.type0 <- coef.gva
  tax.type0 <- coef.tax
  coe.type0 <- coef.coe
  gos.type0 <- coef.gos

  gva.type1 <- as.vector(coef.gva %*% L1 )
  coe.type1 <- as.vector(coef.coe %*% L1 )
  gos.type1 <- as.vector(coef.gos %*% L1 )
  tax.type1 <- as.vector(coef.tax %*% L1 )

  ### EMPLOYMENT MULTIPLIERS

  coef.emp <- as.numeric(coefs[,"empl_coef"])

  emp.type0 <- coef.emp

  emp.type1 <- as.vector(coef.emp %*% L1)




  ## (note that the method of getting multipliers for GVA and employment is consistent with the
  ## output multiplier method i.e. summing up the columns of L1 is equivalent to multiplying L1
  ## by a vector of 1s - such a vector would be the output coefficients i.e. output/output = 1)

  multipliers <- data.table(supply,
                            output.type0,output.type1,
                            emp.type0,emp.type1,
                            gva.type0,gva.type1,
                            coe.type0,coe.type1,
                            gos.type0,gos.type1)

  return(list(multipliers = multipliers,
              leontief1 = L1))


}
