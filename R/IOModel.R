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
#' @param govt_exp  Numeric. Change in government spending.
#' @param hhold_saving Numeric. Assumed household savings rate.
#' @param hhold_reallocate Character. Name of the vector which determines how household spending is redistributed.
#' @param govt_reallocate Character. Name of the vector which determines how government spending is redistributed.
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
                     hhold_exp,
                     govt_exp,
                     hhold_saving = 0,
                     hhold_reallocate = "hhfce_noalctob",
                     govt_reallocate = "central") {

  ### 1) Prepare the changes in final demand vector

  fdemand <- tobalciomodel::PrepFinalDemand(hhold_exp = hhold_exp,
                                            govt_exp = govt_exp,
                                            hhold_saving = hhold_saving,
                                            hhold_vector = hhold_reallocate,
                                            govt_vector = govt_reallocate,
                                            FAI = FAI)

  ### 2) Read in the input-output tables

  sut <- ReadSUT(fte = fte, FAI = FAI, year_sut = year_sut)

  ### 3) Construct leontief inverse/multipliers

  leontief <- LeontiefCalc(sut, FAI = FAI)

  ### 4) Calculate economic impacts

  econ_impacts <- EconEffectsCalc(leontief, fdemand, FAI = FAI, year = year)

  ### RETURN OUTPUTS

  effects <- econ_impacts$effects

  return(effects)

}
