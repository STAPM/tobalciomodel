#' Model a Change in Final Demand due to Alcohol Duty
#'
#' Apply a change in the duties on alcohol and calculate the effect on price, consumption,
#' and total change in demand for each alcohol product.
#'
#' @export
TaxPol <- function(
  alcohol_data      = tobalciomodel::data_mesas_englandwales,
  elasticities_data = tobalciomodel::data_elasticities,
  duties_data       = tobalciomodel::data_alcohol_duty,
  beer_duty         = c(1,1,1),
  cider_duty        = c(1,1,1,1,1),
  wine_duty         = c(1,1,1,1,1,1,1),
  spirit_duty       = c(1),
  rtd_duty          = c(1),
  select_year       = 2019,
  allow_cross       = "No"
) {

  ######################################################
  ###### STEP (1) MAKE CHANGES TO THE DUTY PER UNIT DATA
  ######################################################

  # create vector of the duty changes, and repeat for on and off trade
  changes <- rep(c(beer_duty,cider_duty,wine_duty,spirit_duty,rtd_duty),2)
  duties <- cbind(duties_data,changes)

  # convert duties from pence into £
  duties[, duty := duty/100]
  duties[, dutyperlitre := dutyperlitre/100]

  litres <- 100

  # apply the changes in duty per litre of product/pure alcohol
  duties[, perlitres  := ifelse(is.na(dutyperlitre) ,0,1)]
  duties[, of_product := ifelse(how_applied == "per litre of product" ,1,0)]
  duties[, of_alcohol := ifelse(how_applied == "per litre of pure alcohol" ,1,0)]

    duties[perlitres == 1                  , new_dutyperlitre := (1 + changes/100)*dutyperlitre ]

    duties[perlitres == 1 & of_product == 1, new_duty := (new_dutyperlitre)/(litres*abv)]
    duties[perlitres == 1 & of_alcohol == 1, new_duty := (new_dutyperlitre)/litres]

  # apply changes in duty

  duties[perlitres == 0, new_duty := (1 + changes/100)*duty]

  duties <- duties[,c("product","duty","new_duty","marketshare")]

  #### create a product level dataset of duty data

  duties[,duty_per_unit     := weighted.mean(duty,w=marketshare)    , by = "product"]
  duties[,new_duty_per_unit := weighted.mean(new_duty,w=marketshare), by = "product"]
  duties <- duties[,c("product","duty_per_unit","new_duty_per_unit")]
  duties <- unique(duties)
  duties[,ch_duty_per_unit := new_duty_per_unit - duty_per_unit]

  duties[, product := factor(product,
                           levels = c("off_beer","off_cider","off_wine","off_spirits","off_rtds",
                                      "on_beer" ,"on_cider" ,"on_wine" ,"on_spirits" ,"on_rtds"))]






  #########################################################################
  ##### STEP (2) SELECT YEAR OF ALCOHOL DATA AND FILTER NECESSARY VARIABLES
  #########################################################################

  alc <- alcohol_data[alcohol_data$year == select_year,]

  #### reshape into long form

  long <- melt.data.table(alc,
                          id.vars = "year",
                          measure.vars = list(c("units_off_beer", "units_off_cider", "units_off_wine", "units_off_spirits", "units_off_rtds",
                                                "units_on_beer" , "units_on_cider" , "units_on_wine" , "units_on_spirits" ,"units_on_rtds"),
                                              c("price_off_beer", "price_off_cider", "price_off_wine", "price_off_spirits", "price_off_rtds",
                                                "price_on_beer" , "price_on_cider" , "price_on_wine" , "price_on_spirits" ,"price_on_rtds")
                          ),
                          variable.name = "product",
                          value.name = c("units","price"))

  long[, product := factor(product,
                           levels = c(1:10),
                           labels = c("off_beer","off_cider","off_wine","off_spirits","off_rtds",
                                      "on_beer" ,"on_cider" ,"on_wine" ,"on_spirits" ,"on_rtds"))]


  #### merge duties to the price and consumption data and then to the elasticities

  merge1 <- merge.data.table(long,duties,by = "product")

  #########################################################################
  ##### STEP (3) APPLY ELASTICITIES MATRIX ################################
  #########################################################################

  if (allow_cross == "Yes") {
  elasticities <- as.matrix(elasticities_data[cross == "Yes",-c("cross")])
  } else if (allow_cross == "No") {
  elasticities <- as.matrix(elasticities_data[cross == "No",-c("cross")])
  }

  # calculate duty per unit as a proportion of total price per unit
  # formula for the change in price: Prop Ch Price = (duty % of price) * Prop Ch Duty

  merge1[,duty_prop := duty_per_unit/price]
  merge1[,prop_ch_price := duty_prop*(ch_duty_per_unit/duty_per_unit)]

  # formula for the change in consumption:
  #     prop Ch Cons = elasticity * Prop Ch Price
  #     Ch Cons = elasticity * Prop Ch Price * Consumption

  prop_ch_price <- as.matrix(merge1$prop_ch_price)

  prop_ch_cons  <- elasticities %*% prop_ch_price

  merge2 <- data.table(merge1,prop_ch_cons)
  setnames(merge2,"V1","prop_ch_cons")
  merge2[, ch_cons := prop_ch_cons * units]

  ## change in final demand = price * ch consumption (in millions of pounds)

  merge2[, ch_demand := (ch_cons*price)/1000000]

  ## round figures
  merge2[, duty_per_unit     := round(duty_per_unit,4)]
  merge2[, new_duty_per_unit := round(new_duty_per_unit,4)]
  merge2[, ch_duty_per_unit  := round(ch_duty_per_unit,4)]
  merge2[, duty_prop         := round(duty_prop,4)]
  merge2[, prop_ch_price     := round(prop_ch_price,3)]
  merge2[, prop_ch_cons      := round(prop_ch_cons,3)]
  merge2[, ch_demand         := round(ch_demand,3)]

## return the dataset

return(merge2)
}
