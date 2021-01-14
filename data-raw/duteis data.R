#### construct dataset of duty per unit by alcohol product. Needed to calculate
#### the proportionate increase in duty and the share of duty in the overall price
#### for the change in final demand calculations on MESAS data.

library(data.table)
library(readxl)

data_alcohol_duty <- read_excel("data-raw/tax_policy.xlsx",
                   sheet = "baseline",
                   range = "B3:K37",
                   col_names = TRUE)
setDT(data_alcohol_duty)

#data[,duty_per_unit := weighted.mean(duty,w=marketshare), by = "product"]
#data <- data[,c("product","duty_per_unit")]
#data <- unique(data[product != "FM_cigs" & product != "RYO_tob",])

usethis::use_data(data_alcohol_duty,overwrite=TRUE)

