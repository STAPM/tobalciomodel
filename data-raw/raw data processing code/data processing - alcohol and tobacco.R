## The aim of this code is to produce aggregate measures of alcohol and tobacco demand


## run individual alcohol and tobacco script files

source("data-raw/raw data processing code/data processing - alcohol duties price and consumption.R")
source("data-raw/raw data processing code/data processing - tobacco duties price and consumption.R")

## combine and save

tob_alc_data <- rbindlist(list(alcohol_data, tobacco_data))

usethis::use_data(tob_alc_data, overwrite = TRUE)
