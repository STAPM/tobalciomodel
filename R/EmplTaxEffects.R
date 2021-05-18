#' Calculate Employment Tax Effects
#'
#' Uses estimates of the change in employment by sector and data on average annual salaries
#' by sector to calculate estimates of the change in tax revenues from employment - income
#' tax, employee national insurance contributions (NICs), and employer NICs.
#'
#' @param effects Data table. Containing type 0 and type 1 effects on employment
#' by sector. Output of the \code{EconEffectsCalc} function.
#' @param earnings_data Data table. Average annual earnings by sector.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ### calculate economic effects
#'
#' econ_effect <- EconEffectsCalc(leontief,
#'                            final_demand_vec)
#'
#' ### calculate the tax effects
#'
#' taxes <- EmplTaxEffects(effects = econ_effect)
#'
#' }
EmplTaxEffects <- function(effects,
                           earnings_data = tobalciomodel::ashe_earn_cpa) {

  ### tax effects of employment

  empl <- effects[,c("CPA_code","Product","emp_effects_t0","emp_effects_t1")]

  earn <- merge.data.table(empl, tobalciomodel::ashe_earn_cpa,
                         by = c("CPA_code","Product"))

  ## tax parameters (correct as at 18/05/2021)
  # income tax

  pa                <- 12500
  basic_rate        <- 0.20
  higher_threshold  <- 50000
  higher_rate       <- 0.40

  # employer NICs (weekly, so *52 to get annual)

  employer_nic_threshold <- 166*52
  employer_nic_rate      <- 0.1380

  # employee NICs (also weekly, need to annualise)

  employee_nic_threshold1 <- 166*52
  employee_nic_threshold2 <- 962*52

  employee_nic_rate1 <- 0.12
  employee_nic_rate2 <- 0.02

  ## calculate income tax paid per worker by sector based on average wage.
  earn[,taxable_higher_rate := max(0,avg_salary - higher_threshold), by="CPA_code"]
  earn[,taxable_basic_rate  := max(0,avg_salary - pa) - taxable_higher_rate, by="CPA_code"]

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

  ## multiply tax per employee by the employment effects

  earn[, emp_tax_effects_t0 := total_tax*emp_effects_t0]
  earn[, emp_tax_effects_t1 := total_tax*emp_effects_t1]


  return(list(earn_tax_data = earn,
              total_tax_t0 = sum(earn[,"emp_tax_effects_t0"]),
              total_tax_t1 = sum(earn[,"emp_tax_effects_t1"])))

}
