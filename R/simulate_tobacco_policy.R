#' Model Change in Final Demand due to Tobacco Policy
#'
#' Using selected data from the `final_demand_inputs()` function, model the policy-induced change in
#' final demand for tobacco consumption.
#'
#' @param data dataset created by `final_demand_inputs()`
#' @param tob.policy alcohol policy type to be modelled - c("exog","MUP","tax")
#' @param cigs.ch a vector of proportionate consumption changes to be modelled
#'
#' @return A vector of changes in final demand measured in £bn
#'
#' @export

simulate_tobacco_policy <- function(data = NULL,
                                    tob.policy = "exog",
                                    cigs.ch = 0) {

  ## Initialise a vector to store the changes in final demand

  final.demand <- rep(0,106)

  ## Model an Exogenous change to consumption -------

  if (tob.policy == "exog") {

    # add modelled changes into the dataset
    data <- data.frame(data,cigs.ch)

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

  if (alc.policy == "tax") {

  }

  ## Save out results -------

  final.demand[18] <- data2[2]

  direct.effect <- sum(final.demand)

  return(final.demand)
}
