#' Reallocate Government Expenditure
#'
#' A function to allocate increased government expenditure achieved through a duties policy
#' across the 105 CPA sectors.
#'
#' @param expenditure change in consumption, measured in basic prices.
#' @param vector character. The distribution of reallocation of spending to implement.
#' Valid options are the column names of the \code{vectors_hhold} data. The default is to
#' allocate spending pro-rata according to the 2018 distribution of central government spending.
#' @param vector_data data table containing the redistribution vectors.
#'
#'
#' @export
ReallocateGovt <- function(expenditure = 10,
                           vector = "central",
                           vectors_data = tobalciomodel::vectors_govt
) {

  # calculate the amount of expenditure that will be reallocated

  exp <- expenditure

  # select the chosen reallocation vector

  if (vector == "central") {
    v <- as.vector(as.matrix( vectors_data[,3] ))
  } else if (vector == "local") {
    v <- as.vector(as.matrix( vectors_data[,4] ))
  } else if (vector == "total") {
    v <- as.vector(as.matrix( vectors_data[,5] ))
  } else if (vector == "all_pubadmin") {
    v <- as.vector(as.matrix( vectors_data[,6] ))
  } else if (vector == "all_education") {
    v <- as.vector(as.matrix( vectors_data[,7] ))
  } else if (vector == "all_health") {
    v <- as.vector(as.matrix( vectors_data[,8] ))
  } else if (vector == "all_socialwork") {
    v <- as.vector(as.matrix( vectors_data[,9] ))
  } else if (vector == "all_cultural") {
    v <- as.vector(as.matrix( vectors_data[,10] ))
  }

  # redistribute the expenditure along the vector

  govt_exp <- exp*v
  govt_exp <- cbind(vectors_data[,c(1,2)],govt_exp)

  return(govt_exp)
}
