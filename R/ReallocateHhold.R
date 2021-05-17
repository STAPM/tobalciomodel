#' Reallocate Household Expenditure
#'
#' A function to reallocate, based on assumptions about how individuals
#' redistribute spending, the expenditure saved as a result of a policy
#' or exogenous changes in consumer preferences.
#'
#' @param expenditure change in consumption, measured in basic prices.
#' @param saving_rate proportion of saved expenditure that will be saved rather than redistributed.
#' @param vector character. The distribution of reallocation of spending to implement.
#' Valid options are the column names of the \code{vectors_hhold} data. The default is to allocate spending
#' pro-rata according to the 2018 distribution of consumer spending with alcohol and tobacco excluded.
#' @param vector_data data table containing the redistribution vectors.
#' @param mapping data table containing the mapping algorithm from COICOP to CPA.
#'
#'
#' @export
ReallocateHhold <- function(expenditure = -20,
                            saving_rate = 0.1,
                            vector = "hhfce_noalctob",
                            vectors_data = tobalciomodel::vectors_hhold,
                            mapping = tobalciomodel::coicop_cpa_mapping
                ) {

 # calculate the amount of expenditure that will be reallocated

 exp <- -1*(1 - saving_rate)*expenditure

 # select the chosen reallocation vector

 if (vector == "hhfce_all") {
   v <- as.vector(as.matrix( vectors_data[,2] ))
 } else if (vector == "hhfce_noalc") {
   v <- as.vector(as.matrix( vectors_data[,3] ))
 } else if (vector == "hhfce_notob") {
   v <- as.vector(as.matrix( vectors_data[,4] ))
 } else if (vector == "hhfce_noalctob") {
   v <- as.vector(as.matrix( vectors_data[,5] ))
 } else if (vector == "all_hotels") {
   v <- as.vector(as.matrix( vectors_data[,6] ))
 } else if (vector == "all_rec_durables") {
   v <- as.vector(as.matrix( vectors_data[,7] ))
 } else if (vector == "all_rec_services") {
   v <- as.vector(as.matrix( vectors_data[,8] ))
 }

 # redistribute the expenditure along the vector

 new_exp <- exp*v
 new_exp <- cbind(vectors_data[,1],new_exp)

 # map the new expenditure onto the CPA categories

 merge <- merge.data.table(mapping,
                           new_exp,
                           by = "coicop")

 merge[, mapped_exp := new_exp*mapping]

 # sum up within CPA categories

 merge[, total_exp := sum(mapped_exp), by = "CPA_code"]

 # collapse

 final <- unique(merge[,c("CPA_code","Product","total_exp")])

 ### Two discrepancies are caused by merged CPA categories in the
 ### HHFCE table:
 #
 # (1) H53 (postal and courier) and I55 (accommodation services)
 # (2) C27 (electrical equipment) and C28 (machinery and equipment n.e.c)
 #
 #     Go to the final consumption expenditure by households column
 #     in the IOT sheet in "ioat 2017" and construct ratios from the
 #     figures given there to break up the categories. (Hard coded here)

 x <- merge(final, tobalciomodel::CPA,
            by = "CPA_code", sort = FALSE, all = TRUE)

 H53_I55 <- as.numeric(x[CPA_code == "CPA_H53&I55","total_exp"])

 x[CPA_code == "CPA_H53", total_exp := H53_I55*841/(841+4722)]
 x[CPA_code == "CPA_I55", total_exp := H53_I55*4722/(841+4722)]



 C27_C28 <- as.numeric(x[CPA_code == "CPA_C27 & C28","total_exp"])

 x[CPA_code == "CPA_C27", total_exp := C27_C28*(1348)/(1348+753)]
 x[CPA_code == "CPA_C28", total_exp := C27_C28*(753)/(1348+753)]


 setkey(x, CPA_code)
 x <- x[!"CPA_C27 & C28"]
 x <- x[!"CPA_H53&I55"]

 x <- x[,c("CPA_code","Product.y","total_exp")]
 setnames(x,c("Product.y","total_exp"),c("Product","hhold_exp"))

 ## add in the initial change to expenditure on tobacco and alcohol
 x[Product == "Alcoholic beverages  & Tobacco products",
   hhold_exp := hhold_exp + expenditure]

return(x)
}
