### The purpose of this code is to create a dataset of income tax and national insurance parameters for 2010-2020
### https://www.gov.uk/government/collections/tax-structure-and-parameters-statistics

library(readxl)
library(data.table)

## read in raw data and clean

data <- readxl::read_excel(path = "data-raw/income tax and nics.xlsx")

setDT(data)

data[, basic_rate := as.numeric(basic_rate)]
data[, higher_thresh := as.numeric(higher_thresh)]
data[, higher_rate := as.numeric(higher_rate)]
data[, add_thresh := as.numeric(add_thresh)]
data[, add_rate := as.numeric(add_rate)]

inctax_params <- copy(data)

usethis::use_data(inctax_params, overwrite = TRUE)
