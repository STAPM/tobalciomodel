#' Clean Final Demand Data
#'
#' Take the product level changes in final demand and collapse them into a format which can be used
#' by 'FinalDemandVec()' to produce the vector of final demands to be used in the input-output model.
#'
#' @param data data table containing the product level changes in final demand. Must contain the variable 'ch_demand'
#'
#' @export
CleanFinalDemand <- function(
data
){

data[product %in% c("off_beer","off_cider","off_wine","off_spirits","off_rtds"), name := "off"]
data[product %in% c("on_beer","on_cider","on_wine","on_spirits","on_rtds"), name := "on"]
data[product %in% c("FM_cigs","RYO_tob"), name := "tobacco"]
data[, name := factor(name,levels = c("off","on","tobacco"))]

data[, ch_final_demand := sum(ch_demand), by = "name"]
data <- unique(data[,c("name","ch_final_demand")])

data <- data[order(name),]

data <- as.vector(as.matrix(data[,"ch_final_demand"]))

### fixer-upper - currently doesn't model tobacco, but want to produce a vector of length 3,
###               input code that will add a zero to the end of the vector

if (length(data) == 2) {
  data <- c(data,0)
}

return(data)
}
