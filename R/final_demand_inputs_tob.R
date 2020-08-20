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
                                    elasticity = "meng14") {


}
