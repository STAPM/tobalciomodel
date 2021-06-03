#' Calculate Economic Impacts
#'
#' Apply the model parameters to the change in final demand vector in order to
#' model the impact of changes in demand on output, gross value added, and
#' employment.
#'
#' @param leontief List. The output of \code{LeonfiefCalc} containing multipliers and leontief matrices.
#' @param final_demand_vec Data table. The output of \code{PrepFinalDemand}.
#' @param earnings_data Data table. earnings data by sector to match to employment.
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
                            final_demand_vec,
                            earnings_data = tobalciomodel::ashe_earn_cpa) {

  multipliers <- leontief$multipliers
  L1 <- leontief$leontief1
  L0 <- diag(nrow(L1))
  f  <- as.vector(as.matrix(final_demand_vec[,"final_demand"]))

  ### -------Output effects------- ###

  # P-method

  out_effects_t0_p <- as.vector(as.matrix(L0 %*% f))
  out_effects_t1_p <- as.vector(as.matrix(L1 %*% f))

  # M-method

  out_effects_t0_m <- f*multipliers$output.type0
  out_effects_t1_m <- f*multipliers$output.type1

  # build in test to show M and P methods give the same answer

  testthat::expect_equal(sum(out_effects_t0_m),
                         sum(out_effects_t0_p))

  testthat::expect_equal(sum(out_effects_t1_m),
                         sum(out_effects_t1_p))

  ### -------GVA effects------- ###

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

  ### -------Employment effects------- ###

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

  effects <- cbind(final_demand_vec[,c("CPA_code","Product")],
                     out_effects_t0_m,out_effects_t1_m,
                     gva_effects_t0_m,gva_effects_t1_m,
                     emp_effects_t0_m,emp_effects_t1_m,
                   out_effects_t0_p,out_effects_t1_p,
                   gva_effects_t0_p,gva_effects_t1_p,
                   emp_effects_t0_p,emp_effects_t1_p)

  ##############################################################################
  ### Use calculated employment effects to estimate effects on income taxes ####

  earn <- merge.data.table(effects, earnings_data,
                           by = c("CPA_code","Product"))

  ### tax parameters (CORRECT AS AT 18/05/2021)
    # income tax

  personal_allowance     <- 12570
  basic_rate             <- 0.20
  higher_rate_threshold  <- 50270
  higher_rate            <- 0.40

    # employer NICs (weekly, so *52 to get annual)

  employer_nic_threshold <- 170*52
  employer_nic_rate      <- 0.1380

    # employee NICs (also weekly, need to annualise)

  employee_nic_threshold1 <- 184*52
  employee_nic_threshold2 <- 967*52

  employee_nic_rate1 <- 0.12
  employee_nic_rate2 <- 0.02

    # combine into a data table to be exported

  tax_values <- c(personal_allowance, basic_rate, higher_rate_threshold, higher_rate,
                  employer_nic_threshold, employer_nic_rate,
                  employee_nic_threshold1, employee_nic_rate1,
                  employee_nic_threshold2, employee_nic_rate2)

  tax_vars   <- c("personal_allowance", "basic_rate", "higher_rate_threshold", "higher_rate",
                  "employer_nic_threshold", "employer_nic_rate",
                  "employee_nic_threshold1", "employee_nic_rate1",
                  "employee_nic_threshold2", "employee_nic_rate2")

  tax_params <- data.table(tax_vars, tax_values)

  ##########################
  ##### Tax Calculator #####

  ## calculate income tax paid per worker by sector based on average wage.
  earn[,taxable_higher_rate := max(0,avg_salary - higher_rate_threshold), by="CPA_code"]
  earn[,taxable_basic_rate  := max(0,avg_salary - personal_allowance) - taxable_higher_rate, by="CPA_code"]

  earn[, income_tax := basic_rate*taxable_basic_rate +
         higher_rate*taxable_higher_rate]

  earn[, c("taxable_basic_rate","taxable_higher_rate") := NULL]

  ## calculate employer national insurance contributions

  earn[, employer_nic_elig := max(0,avg_salary - employer_nic_threshold), by="CPA_code"]
  earn[, employer_nic := employer_nic_rate*employer_nic_elig]

  earn[, c("employer_nic_elig") := NULL]

  ## calculate employee national insurance contributions
  earn[,empl_nic_elig2 := max(0,avg_salary - employee_nic_threshold2), by="CPA_code"]
  earn[,empl_nic_elig1 := max(0,avg_salary - employee_nic_threshold1) - empl_nic_elig2, by="CPA_code"]

  earn[, employee_nic := employee_nic_rate1*empl_nic_elig1 +
         employee_nic_rate2*empl_nic_elig2]

  earn[, c("empl_nic_elig1","empl_nic_elig2") := NULL]

  ## total tax per employee

  earn[, total_tax := employee_nic + employer_nic + income_tax]

  ## multiply tax and net earnings per employee by the employment effects

  earn[, emptax_effects_t0_p := total_tax*emp_effects_t0_p/1000000]
  earn[, emptax_effects_t1_p := total_tax*emp_effects_t1_p/1000000]

  earn[, netearn_effects_t0_p := (avg_salary - income_tax - employee_nic)*emp_effects_t0_p/1000000]
  earn[, netearn_effects_t1_p := (avg_salary - income_tax - employee_nic)*emp_effects_t1_p/1000000]

    return(list(effects = earn,
                tax_params = tax_params))

}
