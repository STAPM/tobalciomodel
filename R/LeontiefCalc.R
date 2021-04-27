#' Produce Sectoral Output Multipliers
#'
#' Calculate the Leontief inverse Type I and Type II matrices from the package data and calculate
#' the sector by sector multipliers for output, GVA, and employment.
#'
#' @param FAI logical. TRUE if using the Fraser of Allender Institute IO table (the default). Select FALSE to use one of the ONS tables.
#' @param select_year year of data to use if using the ONS tables.
#'
#' @export
LeontiefCalc <- function(
FAI = TRUE,
select_year = 2010
) {

###  STEP 1) extract the correct input output table and other necessary vectors from package data
if (FAI == TRUE) {
  flowtable <- as.matrix(tobalciomodel::data_iotable_fai[,grep("^[sec]", names(tobalciomodel::data_iotable_fai), value=TRUE)])
  total.output <- as.vector(as.matrix(tobalciomodel::data_iotable_fai[,"total.output"]))
  hhold.demand <- as.vector(as.matrix(tobalciomodel::data_iotable_fai[,"hhold.demand"]))
  hhold.output <- as.vector(as.matrix(tobalciomodel::data_iotable_fai[,"hhold.output"]))
  gva.total    <- as.vector(as.matrix(tobalciomodel::data_iotable_fai[,"gva.total"]))
  gva.taxes    <- as.vector(as.matrix(tobalciomodel::data_iotable_fai[,"gva.taxes"]))
  gva.wages    <- as.vector(as.matrix(tobalciomodel::data_iotable_fai[,"gva.wages"]))
  gva.gos      <- as.vector(as.matrix(tobalciomodel::data_iotable_fai[,"gva.gos"]))
} else if (FAI == FALSE) {
  flowtable <- as.matrix(tobalciomodel::data_iotables_ons[year == select_year,-c(1,107:117)])
  total.output <- as.vector(as.matrix(tobalciomodel::data_iotables_ons[year == select_year,"total.output"]))
  hhold.demand <- as.vector(as.matrix(tobalciomodel::data_iotables_ons[year == select_year,"hhold.demand"]))
  hhold.output <- as.vector(as.matrix(tobalciomodel::data_iotables_ons[year == select_year,"hhold.output"]))
  gva.total    <- as.vector(as.matrix(tobalciomodel::data_iotables_ons[year == select_year,"gva.total"]))
  gva.taxes    <- as.vector(as.matrix(tobalciomodel::data_iotables_ons[year == select_year,"gva.taxes"]))
  gva.wages    <- as.vector(as.matrix(tobalciomodel::data_iotables_ons[year == select_year,"gva.wages"]))
  gva.gos      <- as.vector(as.matrix(tobalciomodel::data_iotables_ons[year == select_year,"gva.gos"]))
}

### STEP 2) Create the Leontief type 1 matrix

## Calculate coefficient matrix:
A <- flowtable %*% ((total.output )^-1 * diag(length(total.output)))
# A <- round(A,5)
# Show A

# Identity matrix minus A
IminusA <- diag(length(total.output)) - A

# Calculate the Leontief Inverse matrix
L1 <- solve(IminusA)
L1 <- round(L1,4)


## STEP 3) Create the Leontief type 2 matrix - endoegenous households
# create a new flowtable - add the employee output/compensation
# as an extra row and household demand as an extra columns
# the household-household transfer on the lead diagonal should be zero
# (households do not supply/demand from/to each other directly)

# in calculating A, the household consumption column is a fraction of total household
# spending

total.hhold.income <- sum(hhold.demand)


flow.table2 <- rbind(flowtable,hhold.output)        # add employment earnings as an extra row
flow.table2 <- cbind(flow.table2,c(hhold.demand,0))  # add household spending as an extra column (0 for no inter-household purchases)

## Calculate coefficient matrix:
A2 <- flow.table2 %*% (( c(total.output,total.hhold.income) )^-1 * diag(length( c(total.output,total.hhold.income)  ) ) )
# A2 <- round(A2,5)
# Identity matrix minus A
IminusA2 <- diag(length(c(total.output,sum(total.output)))) - A2

# Calculate the Leontief Inverse matrix
L2 <- solve(IminusA2)
L2 <- round(L2,4)

### STEP 4) Calculate Sectoral Output Multipliers
# type 0 - direct effect (i.e. no multiplier, just equal to demand change)
# type 1 - direct + indirect effect
# type 2 - direct + indirect + induced effect

output.multipliers.type0 <- rep(1,length(total.output))
output.multipliers.type1 <- rep(NA,length(total.output))
#output.multipliers.type2 <- rep(NA,length(total.output))
for (i in 1:length(total.output)) {
  output.multipliers.type1[i] <- sum(L1[,i])
  #output.multipliers.type2[i] <- sum(L2[,i])
}

### STEP 5) Output to GVA coefficients and multipliers
### (proportion of output comprised of each GVA measure)

coef.gva <- gva.total / total.output
coef.tax <- gva.taxes / total.output
coef.coe <- gva.wages / total.output
coef.gos <- gva.gos   / total.output

gva.multipliers.type0 <- coef.gva
tax.multipliers.type0 <- coef.tax
coe.multipliers.type0 <- coef.coe
gos.multipliers.type0 <- coef.gos

gva.multipliers.type1 <- as.vector(coef.gva %*% L1 / coef.gva)
coe.multipliers.type1 <- as.vector(coef.coe %*% L1 / coef.coe)
gos.multipliers.type1 <- as.vector(coef.gos %*% L1 / coef.gos)
tax.multipliers.type1 <- as.vector(coef.tax %*% L1 / coef.tax)

### STEP 6) Construct the output table

if (FAI == TRUE) {
  sector <- as.character(tobalciomodel::data_iotable_fai$name)
} else if (FAI == FALSE) {
  sector <- as.character(tobalciomodel::data_iotables_ons[year == select_year,"name"])
}

data <- cbind(sector,
              output.multipliers.type0,output.multipliers.type1,
              gva.multipliers.type0,gva.multipliers.type1,
              coe.multipliers.type0,coe.multipliers.type1,
              gos.multipliers.type0,gos.multipliers.type1,
              tax.multipliers.type0,tax.multipliers.type1)

data <- data.table(data)

data$output.multipliers.type0 <- as.numeric(as.character(data$output.multipliers.type0))
data$output.multipliers.type1 <- as.numeric(as.character(data$output.multipliers.type1))

data$gva.multipliers.type0 <- as.numeric(as.character(data$gva.multipliers.type0))
data$gva.multipliers.type1 <- as.numeric(as.character(data$gva.multipliers.type1))

data$tax.multipliers.type0 <- as.numeric(as.character(data$tax.multipliers.type0))
data$tax.multipliers.type1 <- as.numeric(as.character(data$tax.multipliers.type1))

data$coe.multipliers.type0 <- as.numeric(as.character(data$coe.multipliers.type0))
data$coe.multipliers.type1 <- as.numeric(as.character(data$coe.multipliers.type1))

data$gos.multipliers.type0 <- as.numeric(as.character(data$gos.multipliers.type0))
data$gos.multipliers.type1 <- as.numeric(as.character(data$gos.multipliers.type1))

 return(list(multipliers = data,
             leontief1 = L1,
             leontief2 = L2))
}
