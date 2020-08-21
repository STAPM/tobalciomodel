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
  ## Initialise scalars of change in tobacco spending by policy type

  final.demand <- rep(0,106)

  data.exog <- 0
  data.mup <- 0
  data.tax <- 0

  ## Model an Exogenous change to consumption -------

  if (tob.policy == "exog") {

    # add modelled changes into the dataset
    data <- data.frame(data,cigs.ch)

    data.exog <- data %>%
      dplyr::mutate(E.Change = Expenditure*cigs.ch) %>%
      dplyr::select(E.Change) %>%
      dplyr::mutate(tot.change = sum(E.Change)) %>%
      dplyr::select(tot.change) %>%
      dplyr::distinct()
    data.exog <- as.vector(as.matrix(data.exog))
  }

  ## Model a minimum unit price (MUP) -------

  if (tob.policy == "MUP") {

  }

  ## Model a tax change -------

  if (tob.policy == "tax") {

  }

  ## Save out results -------

  final.demand[18] <- data.exog + data.mup + data.tax

  direct.effect <- sum(final.demand)

  return(final.demand)
}
