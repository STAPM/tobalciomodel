#' Calculate Economic Impacts
#'
#' Apply the model parameters to the change in final demand vector in order to
#' model the impact of changes in demand on output, gross value added, and
#' employment.
#'
#' @param leontief List object. The output of \code{LeonfiefCalc} containing multipliers and leontief matrices.
#' @param fdemand Data table. The output of \code{PrepFinalDemand}.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#' @param year Integer. Select the year (from 2016 to 2020) of alcohol, tobacco, earnings and employment data to use in the analysis.
#' @param tax_data Data table. Package data containing the necessary parameters to calculate
#' income tax and national insurance contributions from annual earnings data
#'
#' @return A data table of economic impacts by sector
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' }
EconEffectsCalc <- function(leontief,
                            fdemand,
                            FAI = FALSE,
                            year = 2019,
                            tax_data = tobalciomodel::inctax_params) {

  yr <- copy(year)

  ## vector of annual earnings by sector based on choice of IO tables

  if (FAI == TRUE) {
    earnings_data <- tobalciomodel::ashe_earn_fai[year == yr,]
  } else if (FAI == FALSE) {
    earnings_data <- tobalciomodel::ashe_earn_cpa[year == yr,]
  }

  ## extract multipliers and final demand vectors from inputs

  multipliers <- leontief$multipliers
  L2 <- leontief$leontief2
  L1 <- leontief$leontief1
  L0 <- diag(nrow(L1))
  f  <- as.vector(as.matrix(fdemand[,"final_demand"]))
  f2 <- c(f, 0)

  #### Calculate economic effects here - using both the "p" and "m" methods
  #### p -> "product" method - matrix multiplication.
  #### m -> "multiplier" method - use the multipliers.

  #### Both methods will yield the same aggregate economic effects, but the interpretation
  #### of the calculations at the sector level have different interpretations:

  #### "p method" - the economic impact of all changes in the economy ON the sector itself.
  #### "m method" - the economic impact of changes in the sector ON THE ECONOMY AS A WHOLE.


  ####################################
  ### -------Output effects------- ###
  ####################################

  # P-method

  out_effects_t0_p <- as.vector(as.matrix(L0 %*% f))
  out_effects_t1_p <- as.vector(as.matrix(L1 %*% f))
  out_effects_t2_p <- as.vector(as.matrix(L2 %*% f2))

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

  effects <- cbind(fdemand[,-c("hhold_exp","govt_exp","final_demand")],
                     out_effects_t0_m,out_effects_t1_m,
                     gva_effects_t0_m,gva_effects_t1_m,
                     emp_effects_t0_m,emp_effects_t1_m,
                   out_effects_t0_p,out_effects_t1_p,
                   gva_effects_t0_p,gva_effects_t1_p,
                   emp_effects_t0_p,emp_effects_t1_p)

  ##############################################################################
  ### Use calculated employment effects to estimate effects on income taxes ####

  ### read in the earnings data for the IO table

  if (FAI == TRUE) {
  earn <- merge.data.table(effects, earnings_data,
                           by = c("IOC","Sector"), sort = FALSE)
  } else if (FAI == FALSE) {
  earn <- merge.data.table(effects, earnings_data,
                           by = c("CPA_code","Product"), sort = FALSE)
  }

  ##########################################################
  ### Extract the tax parameters for the analysis year #####

  tax_data <- tax_data[year == yr,]

    # income tax

  personal_allowance     <- as.numeric(tax_data[,"personal_allowance"])
  basic_rate             <- as.numeric(tax_data[,"basic_rate"])
  higher_rate_threshold  <- as.numeric(tax_data[,"higher_thresh"])
  higher_rate            <- as.numeric(tax_data[,"higher_rate"])

    # employer NICs (weekly, so *52 to get annual)

  employer_nic_threshold <- as.numeric(tax_data[,"employer_nic_thresh_wk"])*52
  employer_nic_rate      <- as.numeric(tax_data[,"employer_nic_rate"])

    # employee NICs (also weekly, need to annualise)

  employee_nic_threshold1 <- as.numeric(tax_data[,"employee_nic_thresh1_wk"])*52
  employee_nic_threshold2 <- as.numeric(tax_data[,"employee_nic_thresh2_wk"])*52

  employee_nic_rate1 <- as.numeric(tax_data[,"employee_nic_rate1"])
  employee_nic_rate2 <- as.numeric(tax_data[,"employee_nic_rate2"])

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

  # temp rename sector identifier so the calculator runs for either IO table setup
  if (FAI == TRUE) {
    setnames(earn, "IOC", "code")
    } else if (FAI == FALSE) {
    setnames(earn, "CPA_code", "code")
  }
        ########################################################################
        ## calculate income tax paid per worker by sector based on average wage.
  earn[,taxable_higher_rate := max(0,avg_salary - higher_rate_threshold), by="code"]
  earn[,taxable_basic_rate  := max(0,avg_salary - personal_allowance) - taxable_higher_rate, by="code"]

  earn[, income_tax := basic_rate*taxable_basic_rate +
         higher_rate*taxable_higher_rate]

  earn[, c("taxable_basic_rate","taxable_higher_rate") := NULL]
        ########################################################################
        ## calculate employer national insurance contributions

  earn[, employer_nic_elig := max(0,avg_salary - employer_nic_threshold), by="code"]
  earn[, employer_nic := employer_nic_rate*employer_nic_elig]

  earn[, c("employer_nic_elig") := NULL]

        ########################################################################
        ## calculate employee national insurance contributions

  earn[,empl_nic_elig2 := max(0,avg_salary - employee_nic_threshold2), by="code"]
  earn[,empl_nic_elig1 := max(0,avg_salary - employee_nic_threshold1) - empl_nic_elig2, by="code"]

  earn[, employee_nic := employee_nic_rate1*empl_nic_elig1 +
         employee_nic_rate2*empl_nic_elig2]

  earn[, c("empl_nic_elig1","empl_nic_elig2") := NULL]

  ############################
  ## total tax per employee

  earn[, total_tax := employee_nic + employer_nic + income_tax]

  ########################################################################
  ## multiply tax and net earnings per employee for each sector by the
  ## sectors employment effects

  earn[, emptax_effects_t0_p := total_tax*emp_effects_t0_p/1000000]
  earn[, emptax_effects_t1_p := total_tax*emp_effects_t1_p/1000000]

  earn[, netearn_effects_t0_p := (avg_salary - income_tax - employee_nic)*emp_effects_t0_p/1000000]
  earn[, netearn_effects_t1_p := (avg_salary - income_tax - employee_nic)*emp_effects_t1_p/1000000]

  ## replace the name of the sector variable

  if (FAI == TRUE) {
    setnames(earn, "code", "IOC")
  } else if (FAI == FALSE) {
    setnames(earn, "code", "CPA_code")
  }

  ## remove redundant variables

  earn[, c("avg_salary","total_tax","income_tax","employee_nic","employer_nic") := NULL]

    return(list(effects = earn,
                tax_params = tax_params))

}
