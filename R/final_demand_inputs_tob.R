#' Preparation to Model Change in Tobacco Final Demand
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

final_demand_inputs_tob <- function(yr = 2010,
                                    elasticity = "czubeck10") {

  #### (1) Create Data Table of Data/Parameter Inputs

  ## Filter the elasticities data based on chosen source

  elasticity_data <- as.data.table(tobalciomodel::elasticities_tob)

  if (elasticity == "gallus06") {
    elasticity_data <- elasticity_data[1,]
  } else if (elasticity == "czubeck10") {
    elasticity_data <- elasticity_data[2,]
  }

  ## Filter the Tobacco data by year

  tobacco <- as.data.table(tobalciomodel::tobacco_data) %>%
      dplyr::filter(year == yr)

  ## merge elasticities and tobacco data

  data <- cbind(tobacco,elasticity_data) %>%
    dplyr::mutate(Expenditure = expenditure/1000000000) %>%
    dplyr::mutate(Consumption = consumption/1000000000) %>%
    dplyr::rename(Price = price.per.cig) %>%
    dplyr::select(year,Expenditure,Consumption,Price,Elasticity)


  return(data)
}
