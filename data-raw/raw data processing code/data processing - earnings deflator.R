### The purpose of this code is to create an earnings deflator from the ONS
### Average Weekly Earnings (AWE) Index from:
### https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/timeseries/ka5h/emp

library(readxl)
library(data.table)

## read in raw data and clean

awe <- readxl::read_excel(path = "data-raw/series-021121-average-weekly-earnings.xls")

awe <- awe[8:28,]

setDT(awe)

setnames(awe, names(awe), c("year","awe_index"))

awe[, awe_index := as.numeric(awe_index)]
awe[, year := as.numeric(year)]

## rebase to 2020

base <- as.numeric(awe[year == 2020,"awe_index"])

awe[, awe_index := 100*(awe_index/base)]



usethis::use_data(awe, overwrite = TRUE)
