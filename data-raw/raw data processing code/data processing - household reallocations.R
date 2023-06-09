library(readxl)
library(data.table)

#####################################################
###### Household final consumption expenditure ######


## distribution of spending across the 36 products
# (i) an overall distribution
# (ii) exclude alcohol
# (iii) exclude tobacco
# (iv) exclude both

year <- 2018

totals <- read_excel("data-raw/supply and use 1997-2018.xlsx",
                     sheet = paste0("Table 3 - HHFCe ",year),
                     range = "A108:AM108",
                     col_names = FALSE)


hhfce <- read_excel("data-raw/supply and use 1997-2018.xlsx",
                    sheet = paste0("Table 3 - HHFCe ",year),
                    range = "A4:AL107",
                    col_names = TRUE)

#setDT(hhfce)


#### (1) Calculate amount of expenditure to reallocate

#reallocation <- -1*(1 - savings_rate)*change_exp

#### (2a) Calculate the distribution of consumption across the 36 COICOP products

prop_all           <- totals
prop_noalc         <- totals
prop_notob         <- totals
prop_noalctob      <- totals
prop_noalctob_disp <- totals

for (i in 3:39) {

  prop_all[,i]           <- totals[,i]/(totals[,39])
  prop_noalc[,i]         <- totals[,i]/(totals[,39] - totals[,5])
  prop_notob[,i]         <- totals[,i]/(totals[,39] - totals[,6])
  prop_noalctob[,i]      <- totals[,i]/(totals[,39] - totals[,5] - totals[,6])
  prop_noalctob_disp[,i] <- totals[,i]/(totals[,39] - totals[,5] - totals[,6] -
                                        totals[,10] - totals[,11] - totals[,12] -
                                        totals[,13] - totals[,14] - totals[,21] -
                                        totals[,22] - totals[,23] - totals[,36] )

}

prop_noalc[,5] <- 0
prop_notob[,6] <- 0
prop_noalctob[,c(5,6)] <- 0
prop_noalctob_disp[, c(5,6,10:14,21:23,36)] <- 0

hhfce_all           <- as.vector(as.matrix(prop_all[,-c(1,2,39)]))
hhfce_noalc         <- as.vector(as.matrix(prop_noalc[,-c(1,2,39)]))
hhfce_notob         <- as.vector(as.matrix(prop_notob[,-c(1,2,39)]))
hhfce_noalctob      <- as.vector(as.matrix(prop_noalctob[,-c(1,2,39)]))
hhfce_noalctob_disp <- as.vector(as.matrix(prop_noalctob_disp[,-c(1,2,39)]))


#### vectors which allocate to one particular product

## hotels/restaurants
#all_hotels       <- c(rep(0,34),1,0)
#all_rec_durables <- c(rep(0,28),1,rep(0,7))
#all_rec_services <- c(rep(0,30),1,rep(0,5))

#all_clothing     <- c(rep(0,5),1,rep(0,30))
#all_furniture    <- c(rep(0,12),1,rep(0,23))
#all_appliances   <- c(rep(0,14),1,rep(0,21))
#all_vehicles     <- c(rep(0,21),1,rep(0,14))
#all_books        <- c(rep(0,31),1,rep(0,4))

#### combine vectors into one data table

coicop <- names(hhfce[,-c(1:2)])

proportions <- data.table(coicop,
                          hhfce_all,
                          hhfce_noalctob,
                          hhfce_noalctob_disp)
setDT(proportions)

vectors_hhold <- copy(proportions)

usethis::use_data(vectors_hhold, overwrite = TRUE)



########################################################
############# Map COICOP to the CPA Sectors ############
########################################################

rm(prop_all,prop_noalc,prop_notob,prop_noalctob)

### (3a) create the mapping from the COICOP categories to the CPA

tot <- as.vector(as.matrix(totals[,-c(1,2,39)]))
props <- copy(hhfce[,-c(1,2)])

for (i in 1:36) {
  props[,i] <- props[,i]/tot[i]
}

props <- cbind(hhfce[,1:2],props)
setDT(props)
setnames(props,"...1","CPA_code")

coicop_cpa_mapping <- melt(props,
                    id.vars = c("CPA_code","Product"),
                    variable.name = "coicop",
                    value.name = "mapping")

## no domestic consumption on package holidays so these produce NaN. set to 0


#### ----------- save this as data in the package --------#
coicop_cpa_mapping[coicop == "Package holidays", mapping := 0]
###########################################################

usethis::use_data(coicop_cpa_mapping, overwrite = TRUE)
