### The purpose of this code is to construct aggregate price and consumption data for
### alcohol products to use in aggregate analyses.

library(data.table)
library(readxl)
library(plyr)

rm(list = ls())

prod_list <- c("off_beer","off_cider","off_wine","off_spirits","off_rtds",
               "on_beer","on_cider","on_wine","on_spirits","on_rtds")

##################################
###### Duties

data <- readxl::read_excel("data-raw/alcohol duties 2016-2021.xlsx",
                           range = "B1:I161")

setDT(data)

duties <- data[, .(duty = weighted.mean(duty_per_unit, w = marketshare)), by = c("year","product")]

duties[, product := factor(product, levels = prod_list)]

rm(data)

##################################
############# MESAS

sheet    <- "Scotland data"

r_pop    <- "C23:C27"

r_pr_on  <- "B43:AC52"
r_pr_off <- "AE43:BF52"

r_con_on  <- "B30:AC39"
r_con_off <- "AE30:BF39"

### population

popsize   <- readxl::read_excel("data-raw/mesas-monitoring-report-2021-alcohol-sales.xlsx",
                                sheet = "Population data",
                                range = r_pop,
                                col_names = FALSE)
setDT(popsize)

year <- 2016:2020

setnames(popsize, names(popsize), "population")

pop_data <- data.table(year,popsize)

rm(year, popsize)

### prices

# on-trade

mesas_on  <- readxl::read_excel("data-raw/mesas-monitoring-report-2021-alcohol-sales.xlsx",
                                sheet = sheet,
                                range = r_pr_on)
setDT(mesas_on)

mesas_on  <- mesas_on[, c(1,24:28)]
setnames(mesas_on, names(mesas_on), c("product","2016","2017","2018","2019","2020"))
mesas_on  <- mesas_on[product %in% c("Beer","Cider","Wine","Spirits","RTDs"), ]

mesas_on <- melt(mesas_on,
                 id.vars = "product",
                 variable.name = "year",
                 value.name = "price_per_unit")

mesas_on$product <- plyr::mapvalues(mesas_on$product,
                                    from = c("Beer","Cider","Wine","Spirits","RTDs"),
                                    to = c("on_beer","on_cider","on_wine","on_spirits","on_rtds"))

# off-trade

mesas_off  <- readxl::read_excel("data-raw/mesas-monitoring-report-2021-alcohol-sales.xlsx",
                                 sheet = sheet,
                                 range = r_pr_off)
setDT(mesas_off)

mesas_off  <- mesas_off[, c(1,24:28)]
setnames(mesas_off, names(mesas_off), c("product","2016","2017","2018","2019","2020"))
mesas_off  <- mesas_off[product %in% c("Beer","Cider","Wine","Spirits","RTDs"), ]

mesas_off <- melt(mesas_off,
                  id.vars = "product",
                  variable.name = "year",
                  value.name = "price_per_unit")

mesas_off$product <- plyr::mapvalues(mesas_off$product,
                                     from = c("Beer","Cider","Wine","Spirits","RTDs"),
                                     to = c("off_beer","off_cider","off_wine","off_spirits","off_rtds"))

# combine

prices <- rbindlist(list(mesas_off,mesas_on))

prices[, product := factor(product, levels = prod_list)]
prices <- prices[order(year,product),]

prices[, price_per_unit := as.numeric(price_per_unit)]

rm(mesas_off, mesas_on)

### consumption

# on-trade

mesas_on  <- readxl::read_excel("data-raw/mesas-monitoring-report-2021-alcohol-sales.xlsx",
                                sheet = sheet,
                                range = r_con_on)
setDT(mesas_on)

mesas_on  <- mesas_on[, c(1,24:28)]
setnames(mesas_on, names(mesas_on), c("product","2016","2017","2018","2019","2020"))
mesas_on  <- mesas_on[product %in% c("Beer","Cider","Wine","Spirits","RTDs"), ]

mesas_on <- melt(mesas_on,
                 id.vars = "product",
                 variable.name = "year",
                 value.name = "units_per_adult")

mesas_on$product <- plyr::mapvalues(mesas_on$product,
                                    from = c("Beer","Cider","Wine","Spirits","RTDs"),
                                    to = c("on_beer","on_cider","on_wine","on_spirits","on_rtds"))

# off-trade

mesas_off  <- readxl::read_excel("data-raw/mesas-monitoring-report-2021-alcohol-sales.xlsx",
                                 sheet = sheet,
                                 range = r_con_off)
setDT(mesas_off)

mesas_off  <- mesas_off[, c(1,24:28)]
setnames(mesas_off, names(mesas_off), c("product","2016","2017","2018","2019","2020"))
mesas_off  <- mesas_off[product %in% c("Beer","Cider","Wine","Spirits","RTDs"), ]

mesas_off <- melt(mesas_off,
                  id.vars = "product",
                  variable.name = "year",
                  value.name = "units_per_adult")

mesas_off$product <- plyr::mapvalues(mesas_off$product,
                                     from = c("Beer","Cider","Wine","Spirits","RTDs"),
                                     to = c("off_beer","off_cider","off_wine","off_spirits","off_rtds"))

# combine

cons <- rbindlist(list(mesas_off,mesas_on))

cons[, product := factor(product, levels = prod_list)]
cons <- cons[order(year,product),]

cons[, units_per_adult := as.numeric(units_per_adult)]

rm(mesas_off, mesas_on)

#############################################
########## Merge the datasets together ######

cons[, year := as.numeric(year) + 2015]
prices[, year := as.numeric(year) + 2015]
duties[, year := as.numeric(year)]

data <- merge(prices, cons, by = c("year","product"))
data <- merge(data, pop_data, by = "year")
data <- merge(data, duties, by = c("year","product"))

##############################
## Calculate total consumption

data[, units := population * units_per_adult]

#################################
## Calculate basic price per unit

data[, duty := duty/100 ]
data[, vat  := price_per_unit*(0.2/1.2) ]

data[, tax := vat + duty]

data[, basic_price_per_unit := price_per_unit - tax]

#################################
## Calculate expenditure in both MP and BP

data[, exp_mp := (units * price_per_unit)/1000000]
data[, exp_bp := (units * basic_price_per_unit)/1000000]


######### SAVE OUT THE DATA

mesas_scotland <- copy(data)

usethis::use_data(mesas_scotland, overwrite = TRUE)

rm(list = ls())
