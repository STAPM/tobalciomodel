#' Input-Output Wrapper Function
#'
#' Read in inputs, make calculations, and return clean model outputs for an input-output analysis. Wrapper
#' function for the other functions in the package to implement the full workflow. This function takes parameter
#' inputs determined externally (e.g. from processed outputs obtained via the Sheffield Tobacco and Alcohol Policy Model
#' - STAPM).
#'
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table. If FALSE, uses the
#'            ONS supply and use tables. Defaults to FALSE
#' @param year_sut Integer. Select the year (from 1997 to 2018) ONS supply and use tables to use in the analysis.
#' @param year Integer. Select the year (from 2016 to 2020) of alcohol, tobacco, earnings and employment data to use in the analysis.
#' @param fte Logical. If TRUE (default) use full-time equivalent (FTE) employment, if FALSE
#'            use total employees.
#' @param hhold_exp Numeric vector of length 3. Change in household consumption measured in basic prices for - in order - off-trade alcohol,
#' on-trade alcohol, and tobacco.
#' @param govt_revenue Numeric. change in government revenues, measured in basic prices.
#' @param hhold_passthru Numeric. Assumed household rate of passthrough - the proportion of change in spending which
#' is compensated for in spending on other consumption categories. Defaults to 1 (full passthrough).
#' @param govt_passthru Numeric. Assumed government rate of passthrough - the proportion of change in revenues
#' which are matched by changes in government expenditure. Defaults to 0 (no passthrough).
#' @param hhold_reallocate Numeric (1-3). The distribution of reallocation of spending to implement from the \code{vectors_hhold} data.
#' Option 1 allocates pro-rata across all consumption categories, option 2 excludes alcohol and tobacco consumption, option 3
#' (default) further excludes health, education, rents and utilities.
#' @param govt_reallocate Numeric (1-5). The distribution of reallocation of spending to implement from the \code{vectors_govt} data.
#' Option 1 (default) allocates pro-rata according to the distribution of total government spending,
#' option 2 allocates according to central government spending only, option 3 allocates according
#' to local government only. Option 4 allocates all spending to health, and option 5 allocates all spending
#' to education.
#' @param tax_data Data table. Package data containing the necessary parameters to calculate
#' income tax and national insurance contributions from annual earnings data
#'
#' @author Damon Morris
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' }
IOModel  <- function(FAI = FALSE,
                     year_sut = 2018,
                     year = 2019,
                     fte = TRUE,
                     hhold_exp = c(-100,-100,-100),
                     govt_revenue = 0,
                     hhold_passthru = 1,
                     govt_passthru = 0,
                     hhold_reallocate = 3,
                     govt_reallocate = 1,
                     tax_data = tobalciomodel::inctax_params) {

  #### vector of assumptions to summarise in the output

  assumptions = matrix(c(year, hhold_saving, govt_passthru, hhold_reallocate, govt_reallocate),
                       nrow = 1,
                       dimnames = list(NULL,
                                    c("year","hhold_passthru", "govt_passthru",
                                      "hhold_reallocate", "govt_reallocate")))

  #### print model inputs to the console

  fai_table <- ifelse(FAI == TRUE, "FAI", "ONS")

  cat(crayon::bgGreen("Input-Output Model Parameters:\n"))

  cat(crayon::blue("\tChange in Off-Trade Alcohol Demand:"))
  cat(crayon::green("£",hhold_exp[1],"million\n"))

  cat(crayon::blue("\tChange in On-Trade Alcohol Demand:"))
  cat(crayon::green("£",hhold_exp[2],"million\n"))

  cat(crayon::blue("\tChange in Tobacco Demand:"))
  cat(crayon::green("£",hhold_exp[3],"million\n"))

  cat(crayon::blue("\tAnalysis year;  "))
  cat(crayon::green(year))
  cat(crayon::blue("   I/O Table;  "))
  cat(crayon::green(fai_table,"\n"))
  cat(crayon::blue("   H'hold passthrough rate;  "))
  cat(crayon::green(hhold_saving*100,"%"))
  cat(crayon::blue("   Govt passthrough rate;  "))
  cat(crayon::green(govt_passthru*100,"%\n"))


  cat(crayon::bgGreen("Simulating Wider Economy Effects:\n"))

  ### 1) Prepare the changes in final demand vector

  cat(crayon::blue("\t(1/5) Constructing Final Demand Vector:"))

  fdemand <- tobalciomodel::PrepFinalDemand(hhold_exp = hhold_exp,
                                            govt_revenue = govt_revenue,
                                            hhold_passthru = hhold_passthru,
                                            govt_passthru = govt_passthru,
                                            hhold_reallocate = hhold_reallocate,
                                            govt_reallocate = govt_reallocate,
                                            FAI = FAI)
  cat("\tdone\n")

  ### 2) Read in the input-output tables

  cat(crayon::blue("\t(2/5) Reading Input-Output Table:"))
  sut <- ReadSUT(fte = fte,
                 FAI = FAI,
                 year_sut = year_sut)
  cat("\tdone\n")

  ### 3) Construct leontief inverse/multipliers

  cat(crayon::blue("\t(3/5) Calculating Leontief and Multipliers:"))
  leontief <- LeontiefCalc(sut, FAI = FAI)
  cat("\tdone\n")

  ### 4) Calculate economic impacts

  cat(crayon::blue("\t(4/5) Calculating Economic Impacts:"))
  econ_impacts <- EconEffectsCalc(leontief,
                                  fdemand,
                                  FAI = FAI,
                                  year = year,
                                  tax_data = tax_data)
  cat("\tdone\n")

  ### 5) Process results

  cat(crayon::blue("\t(5/5) Processing Outputs:"))
  results <- ProcessOutputs(data = econ_impacts$effects,
                            macro = tobalciomodel::macro_data,
                            FAI = FAI,
                            year = year)
  cat("\tdone\n")

  ### RETURN OUTPUTS

  output <- list(assumptions    = assumptions,
                 fdemand        = fdemand,
                 raw_impacts    = econ_impacts$effects,
                 aggregate      = results$aggregate,
                 industry       = results$industry)

  return(output)
}
