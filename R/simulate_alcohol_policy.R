#' Model Change in Final Demand due to Alcohol Policy
#'
#' Using selected data from the `final_demand_inputs()` function, model the policy-induced change in
#' final demand for alcohol and tobacco consumption.
#'
#' @param data dataset created by `final_demand_inputs()`
#' @param alc.policy alcohol policy type to be modelled - c("exog","MUP","tax")
#' @param on.trade.ch a vector of proportionate consumption changes to be modelled for Beer, Cider, RTDs, Spirits, Wine
#' @param off.trade.ch a vector of proportionate consumption changes to be modelled for Beer, Cider, RTDs, Spirits, Wine
#' @param reallocate logical; if TRUE then saved expenditures are reallocated to other sectors.
#'
#' @return A vector of changes in final demand measured in £bn
#'
#' @export

simulate_alcohol_policy <- function(data = NULL,
                                alc.policy = "exog",
                                on.trade.ch = c(0,0,0,0,0),
                                off.trade.ch = c(0,0,0,0,0),
                                reallocate = FALSE,
                                prob = FALSE) {

## Initialise a vector to store the changes in final demand

final.demand <- rep(0,106)

## Model an Exogenous change to consumption -------

if (alc.policy == "exog") {

# add modelled changes into the dataset
data <- data.frame(data,on.trade.ch,off.trade.ch)

data2 <- data %>%
  dplyr::mutate(Q.Change.on = Consumption.On*on.trade.ch) %>%
  dplyr::mutate(Q.Change.off = Consumption.Off*off.trade.ch) %>%
  dplyr::mutate(C.Change.on = Q.Change.on*Price.On) %>%
  dplyr::mutate(C.Change.off = Q.Change.off*Price.Off) %>%
  dplyr::select(C.Change.on,C.Change.off) %>%
  dplyr::mutate(tot.change.on = sum(C.Change.on)) %>%
  dplyr::mutate(tot.change.off = sum(C.Change.off)) %>%
  dplyr::select(tot.change.on,tot.change.off) %>%
  dplyr::distinct()
data2 <- as.vector(as.matrix(data2))
}

## Model a minimum unit price (MUP) -------

if (alc.policy == "MUP") {

}

## Model a tax change -------

if (alc.policy == "MUP") {

}

## Save out results -------


# off-trade changes go into sector 61 - Wholesale Trade (Alcohol)

final.demand[61] <- data2[2]

# on-trade changes equally split between 69 - Accommodation (Alcohol) and 71 - Food and Beverage (Alcohol)

if (prob == FALSE) {
final.demand[69] <- data2[1]*0.5
final.demand[71] <- data2[1]*0.5
} else if (prob == TRUE) {
  split <- runif(1)
final.demand[69] <- data2[1]*split
final.demand[71] <- data2[1]*(1-split)
}

return(final.demand)
}
