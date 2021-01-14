#' Model a Change in Final Demand due to Preferences
#'
#' Apply an exogenous change in the demand for alcohol.
#'
#' @export
PrefChanges <- function(
alcohol_data      = tobalciomodel::data_mesas_englandwales,
off_trade_change  = c(-0.1,-0.1,-0.1,-0.1,-0.1),
on_trade_change   = c(-0.1,-0.1,-0.1,-0.1,-0.1),
select_year       = 2019) {

# select year of alcohol data

alc <- alcohol_data[alcohol_data$year == select_year,c("product","price","units_total")]

# organise so they are in product order. off to on, beer, cider, wine, spirit, rtd
alc$product <- factor(alc$product,
                      levels = c("off_beer","off_cider","off_wine","off_spirits","off_rtds",
                                 "on_beer","on_cider","on_wine","on_spirits","on_rtds"))
alc <- alc[order(alc$product),]

# calculate new units

alc[product == "off_beer"   , new_units_total := units_total + units_total*off_trade_change[1]]
alc[product == "off_cider"  , new_units_total := units_total + units_total*off_trade_change[2]]
alc[product == "off_wine"   , new_units_total := units_total + units_total*off_trade_change[3]]
alc[product == "off_spirits", new_units_total := units_total + units_total*off_trade_change[4]]
alc[product == "off_rtds"   , new_units_total := units_total + units_total*off_trade_change[5]]

alc[product == "on_beer"   , new_units_total := units_total + units_total*on_trade_change[1]]
alc[product == "on_cider"  , new_units_total := units_total + units_total*on_trade_change[2]]
alc[product == "on_wine"   , new_units_total := units_total + units_total*on_trade_change[3]]
alc[product == "on_spirits", new_units_total := units_total + units_total*on_trade_change[4]]
alc[product == "on_rtds"   , new_units_total := units_total + units_total*on_trade_change[5]]

# calculate change in demand (in millions)

alc[, ch_demand := price*(new_units_total - units_total)/1000000]

# return the dataset

return(alc)
}

