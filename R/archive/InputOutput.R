#' Input-Output Wrapper Function
#'
#' Read in inputs, make calculations, and return clean model outputs for an input-output analysis. Wrapper
#' function for the other functions in the package to implement the full workflow. This function takes parameter
#' inputs determined externally (e.g. from processed outputs obtained via the Sheffield Tobacco and Alcohol Policy Model
#' - STAPM).
#'
#' @param input_data Data table. Output of the GenInputsSTAPM function.
#' @param write_output Character. File path to the folder to save the IO model outputs.
#' @param treatment_label Character vector. vector of treatment labels identifying specific STAPM model runs.
#' @param tag Character. Additional label to identify this set of input-output models.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table. If FALSE, uses the
#'            ONS supply and use tables. Defaults to FALSE
#' @param year_sut Integer. Select the year (from 1997 to 2018) ONS supply and use tables to use in the analysis.
#' @param year Integer. Select the year (from 2016 to 2020) of alcohol, tobacco, earnings and employment data to use in the analysis.
#' @param fte Logical. If TRUE (default) use full-time equivalent (FTE) employment, if FALSE
#'            use total employees.
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
InputOutput  <- function(input_data = NULL,
                         write_output = NULL,
                         treatment_label = NULL,
                         tag = NULL,
                         FAI = FALSE,
                         year_sut = 2018,
                         year = 2019,
                         fte = TRUE,
                         hhold_passthru = 1,
                         govt_passthru = 0,
                         hhold_reallocate = 3,
                         govt_reallocate = 1,
                         tax_data = tobalciomodel::inctax_params) {

  #### Loop over all models

  for (i in 1:length(treatment_label)){

  data <- input_data[model == treatment_label[i],]

  hhold_exp <- c(as.numeric(data[,"exp_alc_off_bp"]),
                 as.numeric(data[,"exp_alc_on_bp"]),
                 as.numeric(data[,"exp_tob_bp"]) )

  govt_revenue <- as.numeric(data[,"net_govt_revenue"])

  #### vector of assumptions to summarise in the output

  assumptions = matrix(c(year, hhold_passthru, govt_passthru, hhold_reallocate, govt_reallocate),
                       nrow = 1,
                       dimnames = list(NULL,
                                       c("year","hhold_passthru", "govt_passthru",
                                         "hhold_reallocate", "govt_reallocate")))

  #### print model inputs to the console

  fai_table <- ifelse(FAI == TRUE, "FAI", "ONS")

  cat(crayon::bgMagenta("Input-Output Model Parameters:\n"))

  cat(crayon::magenta("\tChange in Off-Trade Alcohol Demand:"))
  cat(crayon::silver("£",hhold_exp[1],"million\n"))

  cat(crayon::magenta("\tChange in On-Trade Alcohol Demand:"))
  cat(crayon::silver("£",hhold_exp[2],"million\n"))

  cat(crayon::magenta("\tChange in Tobacco Demand:"))
  cat(crayon::silver("£",hhold_exp[3],"million\n"))

  cat(crayon::magenta("\tAnalysis year;  "))
  cat(crayon::silver(year))
  cat(crayon::magenta("   I/O Table;  "))
  cat(crayon::silver(fai_table,"\n"))
  cat(crayon::magenta("   H'hold passthrough rate;  "))
  cat(crayon::silver(hhold_passthru*100,"%"))
  cat(crayon::magenta("   Govt passthrough rate;  "))
  cat(crayon::silver(govt_passthru*100,"%\n"))


  cat(crayon::bgMagenta("Simulating Wider Economy Effects:\n"))

  ### 1) Prepare the changes in final demand vector

  cat(crayon::magenta("\t(1/5) Constructing Final Demand Vector:"))

  fdemand <- tobalciomodel::PrepFinalDemand(hhold_exp = hhold_exp,
                                            govt_revenue = govt_revenue,
                                            hhold_passthru = hhold_passthru,
                                            govt_passthru = govt_passthru,
                                            hhold_reallocate = hhold_reallocate,
                                            govt_reallocate = govt_reallocate,
                                            FAI = FAI)
  cat("\tdone\n")

  ### 2) Read in the input-output tables

  cat(crayon::magenta("\t(2/5) Reading Input-Output Table:"))
  sut <- ReadSUT(fte = fte,
                 FAI = FAI,
                 year_sut = year_sut)
  cat("\tdone\n")

  ### 3) Construct leontief inverse/multipliers

  cat(crayon::magenta("\t(3/5) Calculating Leontief and Multipliers:"))
  leontief <- LeontiefCalc(sut, FAI = FAI)
  cat("\tdone\n")

  ### 4) Calculate economic impacts

  cat(crayon::magenta("\t(4/5) Calculating Economic Impacts:"))
  econ_impacts <- EconEffectsCalc(leontief,
                                  fdemand,
                                  FAI = FAI,
                                  year = year,
                                  tax_data = tax_data)
  cat("\tdone\n")

  ### 5) Process results

  cat(crayon::magenta("\t(5/5) Processing Outputs:"))
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

  ### Save to RDS file


  if (is.null(tag)) {
  saveRDS(output, paste0(here::here(write_output), "/iomodel_output" , treatment_label[i],".rds"))


  } else {
  saveRDS(output, paste0(here::here(write_output), "/iomodel_output" , treatment_label[i], "_" , tag ,".rds"))

  }
}
}
