
### This code runs all of the raw data processing R scripts for updating package data

### MAPPING AND CATEGORISING SECTORS

source("data-raw/raw data processing code/data processing - CPA categories info table.R")
source("data-raw/raw data processing code/data processing - map SIC to CPA or IOC.R")

### REALLOCATION VECTORS FOR HOUSEHOLD AND GOVT SPENDING

source("data-raw/raw data processing code/data processing - household reallocations.R")
source("data-raw/raw data processing code/data processing - government reallocations.R")

### INPUT-OUTPUT TABLES

source("data-raw/raw data processing code/data processing - fai iotable.R")
source("data-raw/raw data processing code/data processing - ons supply and use tables.R")

### EARNINGS AND EMPLOYMENT DATA TO MAP TO FAI/CPA CATEGORIES

source("data-raw/raw data processing code/data processing - employment data LFS fai.R")
source("data-raw/raw data processing code/data processing - employment data LFS cpa.R")
source("data-raw/raw data processing code/data processing - earnings data LFS fai.R")
source("data-raw/raw data processing code/data processing - earnings data LFS cpa.R")
source("data-raw/raw data processing code/data processing - earnings deflator.R")
source("data-raw/raw data processing code/data processing - income tax and nics.R")

### AGGREGATE ALCOHOL AND TOBACCO DATA

source("data-raw/raw data processing code/data processing - alcohol duties price and consumption.R")
source("data-raw/raw data processing code/data processing - tobacco duties price and consumption.R")
