#' Allow Households to Adjust Expenditure
#'
#' Using the vector of changes in final demand computed using the simulation functions, allow households
#' to keep their real consumption constant by increasing/decreasing consumption on goods not directly modelled.
#'
#' @param data dataset created by `final_demand_inputs()`
#' @param pro.rata if TRUE, consumption is reallocated according to the existing distribution of
#'                 household spending. If FALSE, manually select sectors to re-allocate to.
#'
#' @return A vector of changes in final demand measured in £bn
#'
#' @export

hhold_reallocate <- function(data = NULL,
                             pro.rata = TRUE,
                             sectors = NULL,
                             prop = NULL) {


redistribute <- round(sum(data),3)*-1

if (pro.rata == TRUE) {

## obtain the distribution of household expenditure
## NOTE this distribution will not sum to 1 because not all hhold expenditure is
## domestically produced goods

distribution <- round(tobalciomodel::iotable$hhold.demand / sum(840117,80917),5)

# obtain total spending to be re-distributed

reallocated <- redistribute*distribution

print(paste0("Proportion of Expenditure Reallocated to Domestic Consumption: ",sum(reallocated,na.rm = TRUE)/redistribute))


} else if (pro.rata == FALSE) {

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

new_demand <- data + reallocated


return(new_demand)
}
