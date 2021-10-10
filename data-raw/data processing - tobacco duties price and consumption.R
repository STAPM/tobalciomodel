### The purpose of this code is to construct aggregate price and consumption data for
### tobacco products to use in aggregate analyses.

library(data.table)
library(readxl)
library(plyr)

rm(list = ls())

##### RYO

data <- readxl::read_excel("data-raw/tobacco data.xlsx",
                          range = "B1:L61",
                          sheet = "RYO_tob")

setDT(data)


ryo <- data[, .(exp_mp = sum(exp_mp),
                tot_tax = sum(Tax)), by = "year"]

ryo[, exp_bp := exp_mp - tot_tax]
ryo[, product := "RYO_tob"]

rm(data)
##### FM

data <- readxl::read_excel("data-raw/tobacco data.xlsx",
                           range = "B1:L61",
                           sheet = "FM_cigs")

setDT(data)


fm <- data[, .(exp_mp = sum(exp_mp),
                tot_tax = sum(Tax)), by = "year"]

fm[, exp_bp := exp_mp - tot_tax]
fm[, product := "FM_cigs"]

rm(data)

##### Combine data sources

tobacco_data <- rbindlist(list(fm, ryo))

usethis::use_data(tobacco_data, overwrite = TRUE)
