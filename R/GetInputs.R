#' Generate IO Model Inputs from Data
#'
#' Function to use aggregate data on expenditures included in the package to estimate
#' changes in expenditure in basic prices following assumed percentage changes in
#' consumption.
#'
#' @param data Data table. Tobacco and alcohol expenditures
#' @param prop_alc_off Proportionate change in alcohol expenditure.
#' @param prop_alc_on Proportionate change in alcohol expenditure.
#' @param prop_tob_fm Proportionate change in factory-made cigarettes expenditure.
#' @param prop_tob_ryo Proportionate change in hand-rolled tobacco expenditure.
#' @param year year of data to use.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
CalcInputs <- function(data = tobalciomodel::tob_alc_data,
                       year = 2019,
                       prop_alc_off = -0.01,
                       prop_alc_on = -0.01,
                       prop_tob_fm = -0.01,
                       prop_tob_ryo = -0.01) {

  yr <- copy(year)

  # restrict to year chosen and split into alcohol and tobacco specific datasets

  data_tob      <- data[year == yr & product %in% c("FM_cigs","RYO_tob"),]
  data_alc      <- data[year == yr & !(product %in% c("FM_cigs","RYO_tob")),]

  data_tob[, tot_tax := exp_mp - exp_bp]
  data_alc[, tot_tax := exp_mp - exp_bp]

  ##############################
  ##### off-trade alcohol ######

  ## expenditure

  data_alc[product %in% c("off_beer","off_cider","off_wine","off_spirits"),
           change := (1 + prop_alc_off)*exp_bp - exp_bp ]
  exp_alc_off_bp <- sum(data_alc[,"change"], na.rm = TRUE)
  data_alc[, change := NULL]

  ## tax

  data_alc[product %in% c("off_beer","off_cider","off_wine","off_spirits"),
           tax_change := (1 + prop_alc_off)*tot_tax - tot_tax ]
  tax_alc_off_bp <- sum(data_alc[,"tax_change"], na.rm = TRUE)
  data_alc[, tax_change := NULL]

  ##############################
  ##### on-trade alcohol #######

  ## expenditure

  data_alc[product %in% c("on_beer","on_cider","on_wine","on_spirits"),
           change := (1 + prop_alc_on)*exp_bp - exp_bp ]
  exp_alc_on_bp <- sum(data_alc[,"change"], na.rm = TRUE)
  data_alc[, change := NULL]

  ## tax

  data_alc[product %in% c("on_beer","on_cider","on_wine","on_spirits"),
           tax_change := (1 + prop_alc_on)*tot_tax - tot_tax ]
  tax_alc_on_bp <- sum(data_alc[,"tax_change"], na.rm = TRUE)
  data_alc[, tax_change := NULL]

  #######################
  ##### Tobacco #########

  ## expenditure

  data_tob[product %in% c("FM_cigs"),
           change := (1 + prop_tob_fm)*exp_bp - exp_bp ]
  data_tob[product %in% c("RYO_tob"),
           change := (1 + prop_tob_ryo)*exp_bp - exp_bp ]

  exp_tob_bp <- sum(data_tob[,"change"], na.rm = TRUE)
  data_tob[, change := NULL]

  ## tax

  data_tob[product %in% c("FM_cigs"),
           tax_change := (1 + prop_tob_fm)*tot_tax - tot_tax ]
  data_tob[product %in% c("RYO_tob"),
           tax_change := (1 + prop_tob_ryo)*tot_tax - tot_tax ]

  tax_tob_bp <- sum(data_tob[,"tax_change"], na.rm = TRUE)
  data_tob[, tax_change := NULL]

  ####################
  ##### Totals #######

  exp_total_bp <- exp_alc_off_bp + exp_alc_on_bp + exp_tob_bp

  net_govt_revenue <- tax_alc_off_bp + tax_alc_on_bp + tax_tob_bp

  ######## Combine outputs

  out <- data.table(year,
                    round(exp_alc_off_bp,3),
                    round(exp_alc_on_bp,3),
                    round(exp_tob_bp,3),
                    round(exp_total_bp,3),
                    round(net_govt_revenue,3))

  setnames(out,
           names(out),
           c("year","exp_alc_off_bp","exp_alc_on_bp","exp_tob_bp",
             "exp_total_bp","net_govt_revenue"))

  return(out)
}
