#' Reallocate Government Revenues
#'
#' Allocate net revenues as a result of a tax policy change to other sectors, reflecting an increase
#' in government spending due to an increase in revenues. Adjust the change in final demand vector to
#' reflect this reallocation before conducting input-output analysis.
#'
#' @param data the data table of duties, prices, and consumption produced by the `TaxPol()` function
#' @param final_demand the vector of final demand changes calcuulated by the `FinalDemandVec()` function.
#' @param FAI logical. TRUE if using the Fraser of Allender Institute IO table (the default). Select FALSE to use one of the ONS tables.
#' @param select_year year of data to use if using the ONS tables.
#'
#' @export
ReallocateGovt <- function(
data = NULL,
final_demand = NULL,
FAI = TRUE,
select_year = 2010
){
# STEP 1) Calculate the change in duty revenues

# government loses the duty on the units no longer consumed and gains the duty increase on units still consumed:
# (duty in pence, convert to pounds by dividing by 100, divide by 1000000 to get into millions of pounds)
data1[, revenue := ((duty_per_unit*ch_cons) + ch_duty_per_unit*(units_total - ch_cons))/(100*1000000) ]

net_revenue <- sum(data1$revenue)

# STEP 2a) Reallocate net tax revenue across final demand

# read in the IO table to get the proportions for pro rata reallocation

if (FAI == TRUE) {
  distribution <- round(tobalciomodel::data_iotable_fai$govt.demand / sum(tobalciomodel::data_iotable_fai$govt.demand),5)
} else if (FAI == FALSE) {
  distribution <- round(tobalciomodel::data_iotables_ons[year == select_year,"govt.demand"] /
                          sum(tobalciomodel::data_iotables_ons[year == select_year,"govt.demand"]),5)
  distribution <- as.vector(as.matrix(distribution))
}

# STEP 2b) manual reallocation

# STEP 3) calculate new final demand vector

new_final_demand = net_revenue*distribution
total_final_demand = new_final_demand + final_demand

return(list(final_demand = total_final_demand,
            net_revenue = net_revenue))
}
