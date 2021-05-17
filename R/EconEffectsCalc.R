#' Calculate Economic Impacts
#'
#' Apply the model parameters to the change in final demand vector in order to
#' model the impact of changes in demand on output, gross value added, and
#' employment.
#'
#' @param leontief List. The output of \code{LeonfiefCalc} containing multipliers and leontief matrices.
#' @param final_demand_vec Data table. The output of \code{PrepFinalDemand}.
#'
#' @return A data table of economic impacts by sector
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ### construct the final demand vector
#'
#' final_demand_vec <- tobalciomodel::PrepFinalDemand(hhold_exp = -20,
#'                                                    govt_exp = 10,
#'                                                    hhold_saving = 0.1,
#'                                                    hhold_vector = "hhfce_noalctob",
#'                                                    govt_vector = "central")
#'
#' ### derive the leontief inverse and multipliers
#'
#' tables <- tobalciomodel::ReadSUT(path = "inputs",
#'                                  year = 2018,
#'                                  fte = TRUE)
#'
#' leontief <- tobalciomodel::LeontiefCalc(tables)
#'
#' ### calculate economic effects
#'
#' effects <- tobalciomodel::EconEffectsCalc(leontief,
#'                                           final_demand_vec)
#'
#' }
EconEffectsCalc <- function(leontief,
                            final_demand_vec) {

L0 <- diag(nrow(L1))
L1 <- mults$leontief1
f  <- as.vector(as.matrix(final_demand_vec[,"final_demand"]))

## output effects: Ch O = L * Ch f

output_effects_t0 <- as.vector(as.matrix(L0 %*% f))
output_effects_t1 <- as.vector(as.matrix(L1 %*% f))

## use the type 0 multipliers - these are equivalent to the
## coefficients and will give the change in the respective quantity in
## the given industry.

emp_effects_t0 <- as.vector(as.matrix(output_effects_t0*mults$multipliers[,"emp.type0"]))
gva_effects_t0 <- as.vector(as.matrix(output_effects_t0*mults$multipliers[,"gva.type0"]))

emp_effects_t1 <- as.vector(as.matrix(output_effects_t1*mults$multipliers[,"emp.type0"]))
gva_effects_t1 <- as.vector(as.matrix(output_effects_t1*mults$multipliers[,"gva.type0"]))

effects <- cbind(finaldemand[,c("CPA_code","Product")],
                 output_effects_t0,output_effects_t1,
                 gva_effects_t0,gva_effects_t1,
                 emp_effects_t0,emp_effects_t1)

setnames(effects,
         names(effects),
         c("CPA_code","Product",
           "output_effects_t0","output_effects_t1",
           "gva_effects_t0","gva_effects_t1",
           "emp_effects_t0","emp_effects_t1"))

return(effects)
}
