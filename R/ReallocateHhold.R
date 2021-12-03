#' Reallocate Household Expenditure
#'
#' A function to reallocate, based on assumptions about how individuals
#' redistribute spending, the expenditure saved as a result of a policy
#' or exogenous changes in consumer preferences.
#'
#' @param expenditure Numeric vector. Change in household consumption measured in basic prices for off-trade alcohol,
#' on-trade alcohol, and tobacco.
#' @param hhold_passthru Numeric. Assumed household rate of passthrough - the proportion of change in spending which
#' is compensated for in spending on other consumption categories. Defaults to 1 (full passthrough).
#' @param vector Numeric (1-3). The distribution of reallocation of spending to implement from the \code{vectors_hhold} data.
#' Option 1 allocates pro-rata across all consumption categories, option 2 excludes alcohol and tobacco consumption, option 3
#' (default) further excludes health, education, rents and utilities.
#' @param vector_data data table containing the redistribution vectors.
#' @param mapping data table containing the mapping algorithm from COICOP to CPA.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table instead of the
#'            ONS ones. Defaults to FALSE.
#'
#'
#' @export
ReallocateHhold <- function(expenditure = c(-20,-10,-30),
                            hhold_passthru = 1,
                            vector = 3,
                            vectors_data = tobalciomodel::vectors_hhold,
                            mapping = tobalciomodel::coicop_cpa_mapping,
                            FAI = T
                ) {

 # calculate the amount of expenditure that will be reallocated

 exp <- -1*(hhold_passthru)*sum(expenditure)

 # select the chosen reallocation vector

 col <- names(vectors_data)[vector+1]
 v <- as.vector(as.matrix( vectors_data[, ..col] ))

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

 #x <- copy(final)

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

 hhold_exp <- copy(x)

 if (FAI == FALSE) {

 ## add in the initial change to expenditure on tobacco and alcohol
 hhold_exp[CPA_code == "CPA_C11.01-6 & C12", hhold_exp := hhold_exp + sum(expenditure)]

 } else if (FAI == TRUE) {

    ## extract the CPA/IOC lookup table and merge, collapsing by FAI categories
    merge_data <- unique(tobalciomodel::sic_cpa_fai_mapping[,c("CPA_code","Product","IOC","Sector")])

    map_to_FAI <- merge(hhold_exp, merge_data, by = c("CPA_code","Product"))

    FAI_data <- map_to_FAI[, .(hhold_exp = sum(hhold_exp)), by = c("IOC","Sector")]

 ## merge to the names of the FAI IO table to get the 3 disaggregated alcohol sectors

    sectors <- as.data.frame(tobalciomodel::iotable_fai[,c("IOC","Sector")])
    setDT(sectors)

    FAI_data <- merge(sectors, FAI_data, by = c("IOC", "Sector"), all = TRUE, sort = FALSE)

 ## fill in the three alcohol categories and manufacture of tobacco with the initial changes
 ## to expenditure - splitting on-trade equally between accommodation/ food and beverage services

    FAI_data[61, hhold_exp := expenditure[1]]
    FAI_data[c(69,71), hhold_exp := 0.5*expenditure[2]]
    FAI_data[18, hhold_exp := expenditure[3]]

 ## merge again to order sector names properly

    hhold_exp <- copy(FAI_data)

 }

 testthat::expect_true(round(sum(hhold_exp$hhold_exp),5) == round(sum(expenditure) -1*(hhold_passthru)*sum(expenditure),5),
                       label = "Function: ReallocateHhold(). Household expenditure vector must sum to total net change in alcohol and tobacco spending reallocated")

return(hhold_exp)
}
