#' Preparation to Model Change in Final Demand
#'
#' This function filters the package data by chosen input parameters and merges the elasticity and MESAS information
#' into a single dataset ready to be used to analyse the effects of policy changes on final demand.
#'
#' @param yr which years data to use?.
#' @param scotland default is FALSE - indicates whether to use Scottish data over the default England/Wales.
#' @param elasticity meng14 or collis10 - the source used for price elasticities.
#'
#' @return A dataset containing price, elasticity, expenditure, and consumption information split
#'         by on and off trade for each of the five alcohol products. Consumption and expenditure measured in billions.
#'
#' @export

final_demand_inputs <- function(yr = 2010,
                                scotland = FALSE,
                                elasticity = "meng14") {

#### (1) Create Data Table of Data/Parameter Inputs

## Filter the elasticities data based on chosen source

elasticity_data <- as.data.table(tobalciomodel::elasticities)

if (elasticity == "meng14") {
elasticity_data <- elasticity_data[1:5,]
} else if (elasticity == "collis10") {
elasticity_data <- elasticity_data[6:10,]
}

## Filter the MESAS data by year and country

 mesas_data <- as.data.table(tobalciomodel::mesas)
 mesas_data$year <- as.numeric(as.character(mesas_data$year))
if (scotland == FALSE) {
mesas_data <- mesas_data %>%
  dplyr::filter(country == "England & Wales" &
                  year == yr &
                  product %in% c("Beer","Cider","Wine","Spirits","RTDs"))
} else if (scotland == TRUE) {
  mesas_data <- mesas_data %>%
    dplyr::filter(country == "Scotland" &
                    year == yr &
                    product %in% c("Beer","Cider","Wine","Spirits","RTDs"))
}

## merge elasticities and MESAS data

data <- merge(mesas_data,elasticity_data,by="product")

data <- data %>%
  dplyr::mutate(Product = product) %>%
  dplyr::mutate(Price.On = price.ontrade) %>%
  dplyr::mutate(Elasticity.On = On.Trade) %>%
  dplyr::mutate(Expenditure.On = consumption.ontrade/1000) %>%
  dplyr::mutate(Consumption.On = (consumption.ontrade/price.ontrade)/1000) %>%
  dplyr::mutate(Price.Off = price.offtrade) %>%
  dplyr::mutate(Elasticity.Off = Off.Trade) %>%
  dplyr::mutate(Expenditure.Off = consumption.offtrade/1000) %>%
  dplyr::mutate(Consumption.Off = (consumption.offtrade/price.offtrade)/1000) %>%
  dplyr::select(Product,Price.On,Elasticity.On,Expenditure.On,Consumption.On,
                Price.Off,Elasticity.Off,Expenditure.Off,Consumption.Off)

return(data)
}
