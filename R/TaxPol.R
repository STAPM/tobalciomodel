#' Model a Change in Alcohol Duties
#'
#' Apply a change in the duties on alcohol and calculate the effect on price, consumption,
#' and total change in demand for each alcohol product.
#'
#' @export
TaxPol <- function(
alcohol_data      = tobalciomodel::data_mesas_englandwales,
elasticities_data = tobalciomodel::elasticities,
duties_data       = tobalciomodel::data_alcohol_duty,
beer_duty         = c(1,1,1),
cider_duty        = c(1,1,1,1,1),
wine_duty         = c(1,1,1,1,1,1,1),
spirit_duty       = c(1),
rtd_duty          = c(1),
select_year       = 2019
) {


  ###### STEP (1) MAKE CHANGES TO THE DUTY PER UNIT DATA

  # create vector of the duty changes, and repeat for on and off trade
  changes <- rep(c(beer_duty,cider_duty,wine_duty,spirit_duty,rtd_duty),2)
  duties <- cbind(duties_data,changes)

  # apply the changes in duty
  duties[, perlitres := ifelse(is.na(dutyper100l),0,1)]

  duties[perlitres == 1, new_dutyper100l := dutyper100l + (changes/100)]
  duties[perlitres == 1, new_duty := (new_dutyper100l*100)/(litres*(100*abv))]

  duties[perlitres == 0, new_duty := duty + changes]

  duties <- duties[,c("product","duty","new_duty","marketshare")]

  #### create a product level dataset of duty data

  duties[,duty_per_unit     := weighted.mean(duty,w=marketshare)    , by = "product"]
  duties[,new_duty_per_unit := weighted.mean(new_duty,w=marketshare), by = "product"]
  duties <- duties[,c("product","duty_per_unit","new_duty_per_unit")]
  duties <- unique(duties)
  duties[,ch_duty_per_unit := new_duty_per_unit - duty_per_unit]


  ##### STEP (2) SELECT YEAR OF ALCOHOL DATA AND FILTER NECESSARY VARIABLES

  alc <- alcohol_data[alcohol_data$year == select_year,c("product","price","units_total")]

  #### merge duties to the price and consumption data and then to the elasticities

  merge1 <- merge.data.table(alc,duties,by = "product")
  merge2 <- merge.data.table(merge1,elasticities_data,by = "product")

  # duties are expressed in pence, prices are in pounds. adjust duty into pounds

  merge2[, duty_per_unit := duty_per_unit/100]
  merge2[, new_duty_per_unit := new_duty_per_unit/100]
  merge2[, ch_duty_per_unit := ch_duty_per_unit/100]

  # (not essential) organise so they are in product order. off to on, beer, cider, wine, spirit, rtd
  merge2$product <- factor(merge2$product,
                           levels = c("off_beer","off_cider","off_wine","off_spirits","off_rtds",
                                      "on_beer","on_cider","on_wine","on_spirits","on_rtds"))

  merge2 <- merge2[order(merge2$product),]

  # calculate duty per unit as a proportion of total price per unit
  merge2[,duty_prop := duty_per_unit/price]

  # formula for the change in price: Prop Ch Price = (duty % of price) * Prop Ch Duty
  merge2[,prop_ch_price := duty_prop*(ch_duty_per_unit/duty_per_unit)]

  # formula for the change in consumption:
  #     prop Ch Cons = elasticity * Prop Ch Price
  #     Ch Cons = elasticity * Prop Ch Price * Consumption

  merge2[, prop_ch_cons := prop_ch_price * elasticity]

  merge2[, ch_cons := prop_ch_cons * units_total]

  ## change in final demand = price * ch consumption (in millions of pounds)

  merge2[, ch_demand := (ch_cons*price)/1000000]

  ## round figures
  merge2[, duty_per_unit <- round(duty_per_unit,4)]
  merge2[, new_duty_per_unit <- round(new_duty_per_unit,4)]
  merge2[, ch_duty_per_unit <- round(ch_duty_per_unit,4)]
  merge2[, duty_prop <- round(duty_prop,4)]
  merge2[, prop_ch_price <- round(prop_ch_price,3)]
  merge2[, prop_ch_cons <- round(prop_ch_cons,3)]
  merge2[, ch_demand <- round(ch_demand,3)]

## return the dataset

return(merge2)
}
