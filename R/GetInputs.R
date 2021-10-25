#' Generate IO Model Inputs from Data
#'
#' Function to use aggregate data on expenditures included in the package to estimate
#' changes in expenditure in basic prices following assumed percentage changes in
#' consumption.
#'
#' @param tobacco Data table. Tobacco expenditures
#' @param alcohol_eng Data table. Alcohol expenditures data for England and Wales.
#' @param alcohol_scot Data table. Alcohol expenditures data for Scotland.
#' @param prop_alc_on Proportionate change in alcohol expenditure.
#' @param prop_alc_off Proportionate change in alcohol expenditure.
#' @param prop_tob_fm Proportionate change in factory-made cigarettes expenditure.
#' @param prop_tob_ryo Proportionate change in hand-rolled tobacco expenditure.
#' @param yr year of data to use.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
GenInputs <- function(tobacco = tobalciomodel::tobacco_data,
                      alcohol_eng = tobalciomodel::mesas_eng_wales,
                      alcohol_scot = tobalciomodel::mesas_scotland,
                      prop_alc_on = -0.1,
                      prop_alc_off = -0.1,
                      prop_tob_fm = -0.1,
                      prop_tob_ryo = -0.1,
                      yr = 2020) {

  # restrict to year chosen
  data_tob      <- tobacco[year == yr,]
  data_alc_eng  <- alcohol_eng[year == yr]
  data_alc_scot <- alcohol_scot[year == yr]

  data_alc <- rbindlist(list(data_alc_eng, data_alc_scot))

  data_alc[, tot_tax := exp_mp - exp_bp]

  ##### off-trade alcohol

  ## expenditure

  data_alc[product %in% c("off_beer","off_cider","off_wine","off_spirits","off_rtds"),
           change := (1 + prop_alc_off)*exp_bp - exp_bp ]
  exp_alc_off_bp <- sum(data_alc[,"change"], na.rm = TRUE)
  data_alc[, change := NULL]

  ## tax

  data_alc[product %in% c("off_beer","off_cider","off_wine","off_spirits","off_rtds"),
           tax := (1 + prop_alc_off)*tot_tax - tot_tax ]
  tax_alc_off_bp <- sum(data_alc[,"tax"], na.rm = TRUE)
  data_alc[, tax := NULL]

  ##### on-trade alcohol

  ## expenditure

  data_alc[product %in% c("on_beer","on_cider","on_wine","on_spirits","on_rtds"),
           change := (1 + prop_alc_on)*exp_bp - exp_bp ]
  exp_alc_on_bp <- sum(data_alc[,"change"], na.rm = TRUE)
  data_alc[, change := NULL]

  ## tax

  data_alc[product %in% c("on_beer","on_cider","on_wine","on_spirits","on_rtds"),
           tax := (1 + prop_alc_on)*tot_tax - tot_tax ]
  tax_alc_on_bp <- sum(data_alc[,"tax"], na.rm = TRUE)
  data_alc[, tax := NULL]

  ##### Tobacco

  ## expenditure

  data_tob[product %in% c("FM_cigs"),
           change := (1 + prop_tob_fm)*exp_bp - exp_bp ]
  data_tob[product %in% c("RYO_tob"),
           change := (1 + prop_tob_ryo)*exp_bp - exp_bp ]

  exp_tob_bp <- sum(data_tob[,"change"], na.rm = TRUE)
  data_tob[, change := NULL]

  ## tax

  data_tob[product %in% c("FM_cigs"),
           tax := (1 + prop_tob_fm)*tot_tax - tot_tax ]
  data_tob[product %in% c("RYO_tob"),
           tax := (1 + prop_tob_ryo)*tot_tax - tot_tax ]

  tax_tob_bp <- sum(data_tob[,"tax"], na.rm = TRUE)
  data_tob[, tax := NULL]

  ##### Totals

  net_govt_revenue <- tax_alc_off_bp + tax_alc_on_bp + tax_tob_bp
  exp_total_bp <- exp_alc_off_bp + exp_alc_on_bp + exp_tob_bp

  ######## Combine outputs

  year = yr

  out <- data.table(year, exp_alc_off_bp, exp_alc_on_bp, exp_tob_bp, exp_total_bp, net_govt_revenue)


  return(out)
}
