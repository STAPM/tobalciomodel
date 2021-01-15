#' Construct the Vector of Final Demand Changes
#'
#' Based on the calculated changes in final demand, construct a vector of final demand changes with the
#' previous calculations as inputs. Calibrate the final demand vector to the input-output table chosen
#' for the analysis.
#'
#' @param data vector of changes in final demand for; off-trade alcohol, on-trade alcohol, tobacco (£m)
#' @param FAI logical. TRUE if using the Fraser of Allender Institute IO table (the default). Select FALSE to use one of the ONS tables.
#'
#' @export
FinalDemandVec <- function(data = NULL,
                           FAI = TRUE) {

  change.off <- data[1]
  change.on  <- data[2]
  change.tob <- data[3]

  if (FAI == TRUE) {
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
  } else if (FAI == FALSE) {
    ## Initialise a vector to store the changes in final demand to put into the IO model
    final.demand <- rep(0,105)

    # off-trade changes go into sector 61 - Wholesale Trade (Alcohol)
    final.demand[59] <- change.off

    # on-trade changes equally split between 69 - Accommodation (Alcohol) and 71 - Food and Beverage (Alcohol)
    split <- 0.5
    final.demand[67] <- change.on[1]*split
    final.demand[68] <- change.on[1]*(1-split)

    # tobacco changes go into sector 18 - manufacture of tobacco products
    final.demand[16] <- change.tob
  }

return(final.demand)
}
