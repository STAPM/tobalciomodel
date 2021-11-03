### The purpose of this code is to construct aggregate price and consumption data for
### alcohol products to use in aggregate analyses. It uses the data files:

# 1) mesas-monitoring-report-2021-alcohol-sales.xlsx    - MESAS (from line 16)
# 2) 2021_Jul_Alc_Tabs.xlsx                             - Alcohol bulletin (from line 390)

library(data.table)
library(readxl)
library(plyr)


##################################
############# MESAS INPUTS

file          <- "mesas-monitoring-report-2021-alcohol-sales"

sheet_s       <- "Scotland data"
sheet_ew      <- "England & Wales data"

r_pop_scot    <- "C17:C27"
r_pop_engw    <- "D17:D27"

r_pr_on       <- "B43:AC52"
r_pr_off      <- "AE43:BF52"

r_con_on      <- "B30:AC39"
r_con_off     <- "AE30:BF39"

min_yr <- 2010
max_yr <- 2020

year_range <- as.character(min_yr:max_yr)

#################################################
######### ENG/WALES DATA ########################

#####################
### POPULATION ######

popsize   <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                                sheet = "Population data",
                                range = r_pop_engw,
                                col_names = FALSE)
setDT(popsize)

year <- min_yr:max_yr

setnames(popsize, names(popsize), "population")

pop_data <- data.table(year,popsize)

rm(year, popsize)

###########################
######## ON-TRADE #########

## get the data on on-trade prices per unit and total units of consumption

## prices

mesas_p  <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                               sheet = sheet_ew,
                               range = r_pr_on)
setDT(mesas_p)

mesas_p  <- mesas_p[, c(1,18:28)]
setnames(mesas_p, names(mesas_p), c("product", year_range))
mesas_p  <- mesas_p[product %in% c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"), ]

mesas_p <- melt(mesas_p, id.vars = "product",  variable.name = "year", value.name = "price_per_unit")

mesas_p[, year := as.numeric(year) + (min_yr-1)]

mesas_p[, price_per_unit := as.numeric(price_per_unit)]

## consumption

mesas_c <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                              sheet = sheet_ew,
                              range = r_con_on)
setDT(mesas_c)

mesas_c  <- mesas_c[, c(1,18:28)]
setnames(mesas_c, names(mesas_c), c("product", year_range))
mesas_c  <- mesas_c[product %in% c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"), ]

mesas_c <- melt(mesas_c, id.vars = "product",  variable.name = "year", value.name = "units_per_adult")

mesas_c[, year := as.numeric(year) + (min_yr-1)]

mesas_c[, units_per_adult := round(as.numeric(units_per_adult),3)]

## merge in population data and calculate total consumption. merge the price and consumption data together.
## Collapse to the four-product level, taking weighted mean of price and summing up total consumption

## population data
mesas_pop <- merge(mesas_c, pop_data, by = "year", all = TRUE)
mesas_pop[, units := units_per_adult*population]
mesas_pop <- mesas_pop[, population := NULL]

mesas <- merge(mesas_p, mesas_pop, by = c("product","year"), all = TRUE, sort = FALSE)

## recode products
mesas$product <- plyr::mapvalues(mesas$product,
                                 from = c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"),
                                 to = c("on_beer","on_cider","on_cider","on_wine","on_wine","on_spirits","on_spirits"))

## collapse to 4-product level

data_on <- mesas[, .(units_per_adult = sum(units_per_adult),
                     units = sum(units),
                     price_per_unit = weighted.mean(price_per_unit,w=units,na.rm=TRUE)),
                 by = c("year","product")]

rm(mesas, mesas_c, mesas_p, mesas_pop)

############################
######## OFF-TRADE #########

## get the data on off-trade prices per unit and total units of consumption

## prices

mesas_p  <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                               sheet = sheet_ew,
                               range = r_pr_off)
setDT(mesas_p)

mesas_p  <- mesas_p[, c(1,18:28)]
setnames(mesas_p, names(mesas_p), c("product", year_range))
mesas_p  <- mesas_p[product %in% c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"), ]

mesas_p <- melt(mesas_p, id.vars = "product",  variable.name = "year", value.name = "price_per_unit")

mesas_p[, year := as.numeric(year) + (min_yr-1)]

mesas_p[, price_per_unit := as.numeric(price_per_unit)]

## consumption

mesas_c <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                              sheet = sheet_ew,
                              range = r_con_off)
setDT(mesas_c)

mesas_c  <- mesas_c[, c(1,18:28)]
setnames(mesas_c, names(mesas_c), c("product", year_range))
mesas_c  <- mesas_c[product %in% c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"), ]

mesas_c <- melt(mesas_c, id.vars = "product",  variable.name = "year", value.name = "units_per_adult")

mesas_c[, year := as.numeric(year) + (min_yr-1)]

mesas_c[, units_per_adult := round(as.numeric(units_per_adult),3)]

## merge in population data and calculate total consumption. merge the price and consumption data together.
## Collapse to the four-product level, taking weighted mean of price and summing up total consumption

## population data
mesas_pop <- merge(mesas_c, pop_data, by = "year", all = TRUE)
mesas_pop[, units := units_per_adult*population]
mesas_pop <- mesas_pop[, population := NULL]

mesas <- merge(mesas_p, mesas_pop, by = c("product","year"), all = TRUE, sort = FALSE)

## recode products
mesas$product <- plyr::mapvalues(mesas$product,
                                 from = c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"),
                                 to = c("off_beer","off_cider","off_cider","off_wine","off_wine","off_spirits","off_spirits"))

## collapse to 4-product level

data_off <- mesas[, .(units_per_adult = sum(units_per_adult),
                      units = sum(units),
                      price_per_unit = weighted.mean(price_per_unit,w=units,na.rm=TRUE)),
                  by = c("year","product")]

rm(mesas, mesas_c, mesas_p, mesas_pop)


#######################################
## COMBINE ON AND OFF TRADE DATASETS ##

engw_data <- rbindlist(list(data_off, data_on))
engw_data[, country := "England & Wales"]

rm(pop_data, data_off, data_on)

#################################################
######### SCOTTISH DATA #########################

#####################
### POPULATION ######

popsize   <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                                sheet = "Population data",
                                range = r_pop_scot,
                                col_names = FALSE)
setDT(popsize)

year <- min_yr:max_yr

setnames(popsize, names(popsize), "population")

pop_data <- data.table(year,popsize)

rm(year, popsize)

###########################
######## ON-TRADE #########

## get the data on on-trade prices per unit and total units of consumption

## prices

mesas_p  <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                                  sheet = sheet_s,
                                  range = r_pr_on)
setDT(mesas_p)

mesas_p  <- mesas_p[, c(1,18:28)]
setnames(mesas_p, names(mesas_p), c("product",year_range))
mesas_p  <- mesas_p[product %in% c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"), ]

mesas_p <- melt(mesas_p, id.vars = "product",  variable.name = "year", value.name = "price_per_unit")

mesas_p[, year := as.numeric(year) + (min_yr-1)]

mesas_p[, price_per_unit := as.numeric(price_per_unit)]

## consumption

mesas_c <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                                sheet = sheet_s,
                                range = r_con_on)
setDT(mesas_c)

mesas_c  <- mesas_c[, c(1,18:28)]
setnames(mesas_c, names(mesas_c), c("product",year_range))
mesas_c  <- mesas_c[product %in% c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"), ]

mesas_c <- melt(mesas_c, id.vars = "product",  variable.name = "year", value.name = "units_per_adult")

mesas_c[, year := as.numeric(year) + (min_yr-1)]

mesas_c[, units_per_adult := round(as.numeric(units_per_adult),3)]

## merge in population data and calculate total consumption. merge the price and consumption data together.
## Collapse to the four-product level, taking weighted mean of price and summing up total consumption

## population data
mesas_pop <- merge(mesas_c, pop_data, by = "year", all = TRUE)
mesas_pop[, units := units_per_adult*population]
mesas_pop <- mesas_pop[, population := NULL]

mesas <- merge(mesas_p, mesas_pop, by = c("product","year"), all = TRUE, sort = FALSE)

## recode products
mesas$product <- plyr::mapvalues(mesas$product,
                                    from = c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"),
                                    to = c("on_beer","on_cider","on_cider","on_wine","on_wine","on_spirits","on_spirits"))

## collapse to 4-product level

data_on <- mesas[, .(units_per_adult = sum(units_per_adult),
                     units = sum(units),
                     price_per_unit = weighted.mean(price_per_unit,w=units,na.rm=TRUE)),
                 by = c("year","product")]

rm(mesas, mesas_c, mesas_p, mesas_pop)

############################
######## OFF-TRADE #########

## get the data on off-trade prices per unit and total units of consumption

## prices

mesas_p  <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                               sheet = sheet_s,
                               range = r_pr_off)
setDT(mesas_p)

mesas_p  <- mesas_p[, c(1,18:28)]
setnames(mesas_p, names(mesas_p), c("product", year_range))
mesas_p  <- mesas_p[product %in% c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"), ]

mesas_p <- melt(mesas_p, id.vars = "product",  variable.name = "year", value.name = "price_per_unit")

mesas_p[, year := as.numeric(year) + (min_yr-1)]

mesas_p[, price_per_unit := as.numeric(price_per_unit)]

## consumption

mesas_c <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                              sheet = sheet_s,
                              range = r_con_off)
setDT(mesas_c)

mesas_c  <- mesas_c[, c(1,18:28)]
setnames(mesas_c, names(mesas_c), c("product", year_range))
mesas_c  <- mesas_c[product %in% c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"), ]

mesas_c <- melt(mesas_c, id.vars = "product",  variable.name = "year", value.name = "units_per_adult")

mesas_c[, year := as.numeric(year) + (min_yr-1)]

mesas_c[, units_per_adult := round(as.numeric(units_per_adult),3)]

## merge in population data and calculate total consumption. merge the price and consumption data together.
## Collapse to the four-product level, taking weighted mean of price and summing up total consumption

## population data
mesas_pop <- merge(mesas_c, pop_data, by = "year", all = TRUE)
mesas_pop[, units := units_per_adult*population]
mesas_pop <- mesas_pop[, population := NULL]

mesas <- merge(mesas_p, mesas_pop, by = c("product","year"), all = TRUE, sort = FALSE)

## recode products
mesas$product <- plyr::mapvalues(mesas$product,
                                 from = c("Beer","Cider","Perry","Wine","Fortified Wines","Spirits","RTDs"),
                                 to = c("off_beer","off_cider","off_cider","off_wine","off_wine","off_spirits","off_spirits"))

## collapse to 4-product level

data_off <- mesas[, .(units_per_adult = sum(units_per_adult),
                     units = sum(units),
                     price_per_unit = weighted.mean(price_per_unit,w=units,na.rm=TRUE)),
                 by = c("year","product")]

rm(mesas, mesas_c, mesas_p, mesas_pop)


#######################################
## COMBINE ON AND OFF TRADE DATASETS ##

scot_data <- rbindlist(list(data_off, data_on))
scot_data[, country := "Scotland"]

rm(pop_data, data_off, data_on)

##############################################################################
################## COMBINE ENGLAND/WALES AND SCOTLAND DATA ###################


data <-rbindlist(list(engw_data, scot_data))
rm(engw_data, scot_data)

## collapse to 4-product/year level. Upshift consumption to be reflective of the total UK
## population (assume NI distribution of alcohol consumption = GB to do so). NI population is 2.84%
## of GBs in each year of ONS population projections from 2018-2030

alcohol_data <- data[, .(units_per_adult = sum(units_per_adult)*1.0284,
                         units = sum(units)*1.0284,
                         price_per_unit = weighted.mean(price_per_unit,w=units,na.rm=TRUE)),
                  by = c("year","product")]
rm(data)

## calculate the total units across on/off trade by each product and calculate the percentage
## split between on/off trade to divide the clearances data

alcohol_data[product %in% c("off_spirits","on_spirits"), prod := "spirits"]
alcohol_data[product %in% c("off_beer","on_beer"), prod := "beer"]
alcohol_data[product %in% c("off_wine","on_wine"), prod := "wine"]
alcohol_data[product %in% c("off_cider","on_cider"), prod := "cider"]


alcohol_data[, total_units := sum(units), by = c("year","prod")]


alcohol_data[, proportion := units/total_units]















##########################################################################
############ READ IN CLEARANCES AND TOTAL DUTIES DATA  ###################

## read in duties for calendar years 2016-2020

spirits  <- readxl::read_excel(paste0("data-raw/","2021_Alcohol_Tables_Spirits",".xlsx"))
beer     <- readxl::read_excel(paste0("data-raw/","2021_Alcohol_Tables_Beer",".xlsx"))
cider    <- readxl::read_excel(paste0("data-raw/","2021_Alcohol_Tables_Cider",".xlsx"))
wine     <- readxl::read_excel(paste0("data-raw/","2021_Alcohol_Tables_Wine",".xlsx"))

## spirits

setDT(spirits)
spirits <- spirits[,c(1,9)]
setnames(spirits, names(spirits), c("year","total_duty"))
spirits <- spirits[year %in% min_yr:max_yr,]
spirits[, prod := "spirits"]

## beer

setDT(beer)
beer <- beer[,c(1,9)]
setnames(beer, names(beer), c("year","total_duty"))
beer <- beer[year %in% min_yr:max_yr,]
beer[, prod := "beer"]

## cider

setDT(cider)
cider <- cider[,c(1,3)]
setnames(cider, names(cider), c("year","total_duty"))
cider <- cider[year %in% min_yr:max_yr,]
cider[, prod := "cider"]

## wine

setDT(wine)
wine <- wine[,c(1,9)]
setnames(wine, names(wine), c("year","total_duty"))
wine <- wine[year %in% min_yr:max_yr,]
wine[, prod := "wine"]

#########################
## COMBINE DUTIES DATA ##

duties <- rbindlist(list(beer, cider, wine, spirits))
rm(beer,cider,wine,spirits)



#################################################################
###### MERGE DUTIES DATA TO MESAS DATA ##########################

data <- merge(alcohol_data, duties, by = c("year","prod"))
rm(alcohol_data)

data[, duty := total_duty*proportion]


data[, total_exp := (price_per_unit * units)/1000000]
data[, vat := total_exp*(0.2/1.2)]

data[, c("total_duty","total_units") := NULL]

data[, total_tax := vat + duty]

data[, exp_bp := total_exp - total_tax]

data[, basic_price_per_unit := (exp_bp*1000000)/units]

setnames(data, c("total_exp","total_tax"), c("exp_mp","tax"))



alcohol_data <- data[, c("year","product","units_per_adult","units","price_per_unit","basic_price_per_unit",
                         "exp_mp","exp_bp","duty","vat","tax")]

#####################################################################
### CONSTRUCT EXPENDITURES IN REAL-TERMS  -  CONSTANT 2020 PRICES ###

real <- alcohol_data[year == 2020, c("price_per_unit","basic_price_per_unit")]

## duplicate data table for each year

real <- rbindlist(list(real, real, real, real, real, real,
                       real, real, real, real, real))
setnames(real,
         c("price_per_unit","basic_price_per_unit"),
         c("price_per_unit_2020","basic_price_per_unit_2020"))

## merge and calculate real expenditures

alcohol_data <- cbind(alcohol_data, real)

alcohol_data[, exp_mp_2020 := (price_per_unit_2020 * units)/1000000]
alcohol_data[, exp_bp_2020 := (basic_price_per_unit_2020 * units)/1000000]

alcohol_data[, unit_define := "1 unit = 10ml of pure alcohol"]


### sort data

alcohol_data <- alcohol_data[,c("year","product","unit_define","duty","vat","units",
                                "price_per_unit","basic_price_per_unit","exp_mp","exp_bp",
                                "price_per_unit_2020","basic_price_per_unit_2020","exp_mp_2020","exp_bp_2020")]

rm(data, duties, real, file, min_yr, max_yr, r_con_off, r_con_on, r_pop_engw, r_pop_scot,
   r_pr_off, r_pr_on, sheet_ew, sheet_s, year_range)

#usethis::use_data(alcohol_data, overwrite = TRUE)
