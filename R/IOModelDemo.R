#' Input-Output Model Example
#'
#' Runs input-output analyses using a simplified 3-sector economy.
#'
#' @param data data table. Package data containing the parameters for the synthetic economy.
#' @param fdemand_vec Numeric vector. A vector of changes in final demand. Must be length 3.
#'
#' @return
#' @export
#'
#' @examples
IOModelDemo  <- function(data = tobalciomodel::demo_data,
                         fdemand_vec = c(0, -1000, 0) ){


### follow the structure of IOModel:

###############################################################################################
### 1) Prepare the changes in final demand vector (no need, directly input to function args)

######################################################################
### 2) Read in the input-output tables (code here from ReadSUT) ######
######################################################################

### io table

iotable <- as.matrix(data[,3:5])

### coefficients

gva.total    <- as.vector(as.matrix(data[,"gva.total"]))
gva.taxes    <- as.vector(as.matrix(data[,"gva.taxes"]))
gva.gos      <- as.vector(as.matrix(data[,"gva.gos"]))
gva.wages    <- as.vector(as.matrix(data[,"gva.wages"]))
total.output <- as.vector(as.matrix(data[,"total.output"]))


employment <- data[, "tot_fte"]

employment <- as.vector(as.matrix(employment))

coefs <- data.table(data$code,
                    data$Sector,
                    total.output,
                    gva.total/total.output,
                    gva.taxes/total.output,
                    gva.gos/total.output,
                    gva.wages/total.output,
                    employment,
                    employment/total.output)

setnames(coefs, names(coefs), c("code","Sector","output",
                                "gva_coef","tax_coef","gos_coef","coe_coef","employment","empl_coef"))

rm(employment, gva.total, gva.gos, gva.taxes, gva.wages, total.output)

####################################################################
### 3) Construct Leontief Inverse and multipliers (LeontiefCalc) ###


total.output <- as.vector(as.matrix(coefs[,"output"]))

## A matrix

A <- iotable %*% ((total.output )^-1 * diag(length(total.output)))

## Leontief Inverse L = (I - A)^-1

L1 <- solve(diag(length(total.output)) - A)
L0 <- diag(nrow(L1)) # for calculating direct effects - an identity matrix

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

coef.gva <- as.vector(as.matrix(coefs[,"gva_coef"]))
coef.tax <- as.vector(as.matrix(coefs[,"tax_coef"]))
coef.coe <- as.vector(as.matrix(coefs[,"coe_coef"]))
coef.gos <- as.vector(as.matrix(coefs[,"gos_coef"]))

gva.type0 <- coef.gva
tax.type0 <- coef.tax
coe.type0 <- coef.coe
gos.type0 <- coef.gos

gva.type1 <- as.vector(coef.gva %*% L1 )
coe.type1 <- as.vector(coef.coe %*% L1 )
gos.type1 <- as.vector(coef.gos %*% L1 )
tax.type1 <- as.vector(coef.tax %*% L1 )

### EMPLOYMENT MULTIPLIERS

coef.emp <- as.vector(as.matrix(coefs[,"empl_coef"]))

emp.type0 <- coef.emp

emp.type1 <- as.vector(coef.emp %*% L1)


## (note that the method of getting multipliers for GVA and employment is consistent with the
## output multiplier method i.e. summing up the columns of L1 is equivalent to multiplying L1
## by a vector of 1s - such a vector would be the output coefficients i.e. output/output = 1)

multipliers <- data.table(data$code,
                          data$Sector,
                          output.type0,output.type1,
                          emp.type0,emp.type1,
                          gva.type0,gva.type1,
                          coe.type0,coe.type1,
                          gos.type0,gos.type1)

rm(output.type0,output.type1, emp.type0,emp.type1, gva.type0,gva.type1,
   coe.type0,coe.type1, gos.type0,gos.type1, tax.type0, tax.type1,
   coef.coe, coef.emp, coef.gos, coef.gva, coef.tax, total.output, i)

#######################################################
### 4) Calculate economic impacts (EconEffectsCalc) ###
#######################################################

earnings_data <- data[,c("code", "Sector", "avg_salary")]

f <- fdemand_vec

####################################
### -------Output effects------- ###
####################################

# P-method

out_effects_t0_p <- as.vector(as.matrix(L0 %*% f))
out_effects_t1_p <- as.vector(as.matrix(L1 %*% f))

# M-method

out_effects_t0_m <- f*multipliers$output.type0
out_effects_t1_m <- f*multipliers$output.type1

# test that M and P methods give the same answer

testthat::expect_equal(sum(out_effects_t0_m),
                       sum(out_effects_t0_p))

testthat::expect_equal(sum(out_effects_t1_m),
                       sum(out_effects_t1_p))

#################################
### -------GVA effects------- ###
#################################

# P-method

gva_effects_t0_p <- multipliers$gva.type0 * out_effects_t0_p
gva_effects_t1_p <- multipliers$gva.type0 * out_effects_t1_p

# M-method

gva_effects_t0_m <- f*multipliers$gva.type0
gva_effects_t1_m <- f*multipliers$gva.type1

# build in test to show M and P methods give the same answer

testthat::expect_equal(sum(gva_effects_t0_m),
                       sum(gva_effects_t0_p))

testthat::expect_equal(sum(gva_effects_t1_m),
                       sum(gva_effects_t1_p))

########################################
### -------Employment effects------- ###
########################################

# P-method

emp_effects_t0_p <- multipliers$emp.type0 * out_effects_t0_p
emp_effects_t1_p <- multipliers$emp.type0 * out_effects_t1_p

# M-method

emp_effects_t0_m <- f*multipliers$emp.type0
emp_effects_t1_m <- f*multipliers$emp.type1

# build in test to show M and P methods give the same answer

testthat::expect_equal(sum(emp_effects_t0_m),
                       sum(emp_effects_t0_p))

testthat::expect_equal(sum(emp_effects_t1_m),
                       sum(emp_effects_t1_p))


## construct data table of outputs

effects <- data.table(data$Sector,
                      out_effects_t0_p,out_effects_t1_p,
                      gva_effects_t0_p,gva_effects_t1_p,
                      emp_effects_t0_p,emp_effects_t1_p)


return(list(iotable = iotable,
            A_matrix = round(A, 3),
            leontief1 = round(L1, 3),
            multipliers = multipliers,
            effects = effects))

}
