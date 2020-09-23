#' Construct the Vector of Final Demand Changes
#'
#' Based on the scenario selected by `tobalciomodel::scenarios`, or manual input, construct a vector of changes
#' in final demand of length 106 and input the changes in off-trade alcohol, on-trade alcohol, and tobacco consumption
#' to be modelled.
#'
#' @param change.off total change in consumption of off-trade alcohol (£m)
#' @param change.on total change in consumption of on-trade alcohol (£m)
#' @param change.tob total change in consumption of tobacco (£m)
#'
#' @export
demand_changes <- function(change.off = NULL,
                           change.on = NULL,
                           change.tob = NULL) {
## Initialise a vector to store the changes in final demand to put into the IO model

final.demand <- rep(0,106)

# off-trade changes go into sector 61 - Wholesale Trade (Alcohol)

final.demand[61] <- change.off

# on-trade changes equally split between 69 - Accommodation (Alcohol) and 71 - Food and Beverage (Alcohol)

split <- 0.5
final.demand[69] <- change.on[1]*split
final.demand[71] <- change.on[1]*(1-split)

# tobacco changes go into sector 18 - manufacture of tobacco products

final.demand[18] <- change.tob


return(final.demand)
}
