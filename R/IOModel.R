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
#' @param hhold_saving Numeric. Assumed household savings rate.
#' @param govt_passthru Numeric. Assumed government rate of passthrough - the proportion of change in revenues
#' (positive or negative) which will be adjusted for in government expenditure. Defaults to 0 - no change to
#' government spending as a result of changes in revenues.
#' @param hhold_reallocate Character. Name of the vector which determines how household spending is redistributed.
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
                     hhold_saving = 0,
                     govt_passthru = 0,
                     hhold_reallocate = 3,
                     govt_reallocate = 1,
                     tax_data = tobalciomodel::inctax_params) {

  fai_table <- ifelse(FAI == TRUE, "FAI", "ONS")

  cat(crayon::bgRed("Input-Output Model Parameters:\n"))

  cat(crayon::magenta("\tChange in Off-Trade Alcohol Demand:"))
  cat(crayon::red("£",hhold_exp[1],"million\n"))

  cat(crayon::magenta("\tChange in On-Trade Alcohol Demand:"))
  cat(crayon::red("£",hhold_exp[2],"million\n"))

  cat(crayon::magenta("\tChange in Tobacco Demand:"))
  cat(crayon::red("£",hhold_exp[3],"million\n"))

  cat(crayon::magenta("\tAnalysis year;  "))
  cat(crayon::red(year))
  cat(crayon::magenta("   I/O Table;  "))
  cat(crayon::red(fai_table,"\n"))
  cat(crayon::magenta("   H'hold saving rate;  "))
  cat(crayon::red(hhold_saving*100,"%"))
  cat(crayon::magenta("   Govt passthrough;  "))
  cat(crayon::red(govt_passthru*100,"%\n"))


  cat(crayon::bgRed("Simulating Wider Economy Effects:\n"))

  ### 1) Prepare the changes in final demand vector

  cat(crayon::yellow("\t(1/5) Constructing Final Demand Vector:"))

  fdemand <- tobalciomodel::PrepFinalDemand(hhold_exp = hhold_exp,
                                            govt_revenue = govt_revenue,
                                            hhold_saving = hhold_saving,
                                            govt_passthru = govt_passthru,
                                            hhold_reallocate = hhold_reallocate,
                                            govt_reallocate = govt_reallocate,
                                            FAI = FAI)
  cat(crayon::white("done\n"))

  ### 2) Read in the input-output tables

  cat(crayon::yellow("\t(2/5) Reading Input-Output Table:"))
  sut <- ReadSUT(fte = fte, FAI = FAI, year_sut = year_sut)
  cat(crayon::white("done\n"))

  ### 3) Construct leontief inverse/multipliers

  cat(crayon::yellow("\t(3/5) Calculating Leontief and Multipliers:"))
  leontief <- LeontiefCalc(sut, FAI = FAI)
  cat(crayon::white("done\n"))

  ### 4) Calculate economic impacts

  cat(crayon::yellow("\t(4/5) Calculating Economic Impacts:"))
  econ_impacts <- EconEffectsCalc(leontief,
                                  fdemand,
                                  FAI = FAI,
                                  year = year,
                                  tax_data = tax_data)
  cat(crayon::white("done\n"))

  ### 5) Process results

  cat(crayon::yellow("\t(5/5) Processing Outputs:"))
  results <- ProcessOutputs(data = econ_impacts$effects,
                            macro = tobalciomodel::macro_data,
                            FAI = FAI,
                            year = year)
  cat(crayon::white("done\n"))

  ### RETURN OUTPUTS

  output <- list(fdemand        = fdemand,
                 raw_impacts    = econ_impacts$effects,
                 aggregate      = results$aggregate,
                 aggregate_perc = results$aggregate_perc,
                 industry       = results$industry,
                 industry_perc  = results$industry_perc)

  return(output)
}
