#' Reallocate Household Spending
#'
#' Allocate household consumption as a result of modelled changes in preferences.
#'
#' @param final_demand the vector of final demand changes calculated by the `FinalDemandVec()` function.
#' @param FAI logical. TRUE if using the Fraser of Allender Institute IO table (the default). Select FALSE to use one of the ONS tables.
#' @param select_year year of data to use if using the ONS tables.
#'
#' @export
ReallocateHHold <- function(
final_demand = NULL,
FAI = TRUE,
select_year = 2010,
prorata = TRUE,
sectors = NULL,
prop = NULL
) {


redistribute <- round(sum(final_demand),3)*-1

if (prorata == TRUE) {

  ## obtain the distribution of household expenditure
  ## NOTE this distribution will not sum to 1 because not all hhold expenditure is
  ## domestically produced goods

  if (FAI == TRUE) {
    distribution <- round(tobalciomodel::data_iotable_fai$hhold.demand / sum(tobalciomodel::data_iotable_fai$hhold.demand),5)
  } else if (FAI == FALSE) {
    distribution <- round(tobalciomodel::data_iotables_ons[year == select_year,"hhold.demand"] /
                            sum(tobalciomodel::data_iotables_ons[year == select_year,"hhold.demand"]),5)
    distribution <- as.vector(as.matrix(distribution))
  }


  # obtain total spending to be re-distributed

  reallocated <- redistribute*distribution

  print(paste0("Proportion of Expenditure Reallocated to Domestic Consumption: ",sum(reallocated,na.rm = TRUE)/redistribute))


} else if (prorata == FALSE) {

  ## produce an error and cancel if the sectors and prop vectors are not of equal length
  if(length(sectors) != length(prop)) stop('sectors and prop vectors must be equal length')

  # empty vector of reallocations to fill in
  reallocated <- rep(NA,106)

  # loop over each chosen reallocation sector
  for (i in sectors) {
    reallocated[i] <- redistribute*prop[i]
  }
  print(paste0("Proportion of Expenditure Reallocated to Domestic Consumption: ",sum(reallocated,na.rm = TRUE)/redistribute))

}

new_demand <- final_demand + reallocated


return(new_demand)
}

