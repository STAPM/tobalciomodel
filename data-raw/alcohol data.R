### Read in the MESAS population, consumption, and price data

library(readxl)
library(dplyr)

#### England and Wales Data ------------------------

## YEAR AND PRODUCT IDENTIFIERS

year <- rep(seq(1994,2019,1),each=9)

product <- rep(c("Total","Spirits","RTDs","Fortified Wines","Wine","Other","Cider","Perry","Beer"),2019-1994+1)

## TOTAL CONSUMED IN LITRES
# consumption data - volume of pure alcohol (L) per adult. (on trade)

litres.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                          sheet = "England & Wales data",
                          range = "C18:AB26",
                          col_names = FALSE)

litres.data <- as.matrix(litres.data)
litres.data <- round(as.numeric(litres.data),2)

litres.pp.ontrade <- as.vector(matrix(litres.data,ncol=1))
rm(litres.data)

# consumption data - volume of pure alcohol (L) per adult. (off trade)

litres.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                          sheet = "England & Wales data",
                          range = "AE18:BD26",
                          col_names = FALSE)

litres.data <- as.matrix(litres.data)
litres.data <- round(as.numeric(litres.data),2)

litres.pp.offtrade <- as.vector(matrix(litres.data,ncol=1))
rm(litres.data)

### UNITS CONSUMED PER PERSON

# consumption data - units per person. (on trade)

units.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "England & Wales data",
                         range = "C31:AB39",
                         col_names = FALSE)

units.data <- as.matrix(units.data)
units.data <- round(as.numeric(units.data),2)

units.pp.ontrade <- as.vector(matrix(units.data,ncol=1))
rm(units.data)

# consumption data - units per person. (off trade)

units.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "England & Wales data",
                         range = "AE31:BD39",
                         col_names = FALSE)

units.data <- as.matrix(units.data)
units.data <- round(as.numeric(units.data),2)

units.pp.offtrade <- as.vector(matrix(units.data,ncol=1))
rm(units.data)

### PRICES PER UNIT

# price data - average price per unit sold (on trade)

prices.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                          sheet = "England & Wales data",
                          range = "C44:AB52",
                          col_names = FALSE)

prices.data <- as.matrix(prices.data)
prices.data <- round(as.numeric(prices.data),2)

price.ontrade <- as.vector(matrix(prices.data,ncol=1))
rm(prices.data)

# price data - average price per unit sold (off trade)

prices.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                          sheet = "England & Wales data",
                          range = "AE44:BD52",
                          col_names = FALSE)

prices.data <- as.matrix(prices.data)
prices.data <- round(as.numeric(prices.data),2)

price.offtrade <- as.vector(matrix(prices.data,ncol=1))
rm(prices.data)

###POPULATION DATA

population <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "Population data",
                         range = "B5:D26",
                         col_names = FALSE)

population <- data.frame(population) %>%
  rename(year = ...1,
         population = ...3) %>%
  select(year,population)

### COMBINE DATA INTO ONE FRAME

data <- data.frame(year,product,
                   litres.pp.ontrade,litres.pp.offtrade,
                   units.pp.ontrade,units.pp.offtrade,
                   price.ontrade,price.offtrade)

## from 2015 impute RTD units per person in the on-trade as equal to the entire "other category"
## (RTD is merged into other with fortified wines and perry from this year onwards. Assume these other 2 = 0)

imp.values.units <- c(data[year==2015 & product == "Other","units.pp.ontrade"],
                      data[year==2016 & product == "Other","units.pp.ontrade"],
                      data[year==2017 & product == "Other","units.pp.ontrade"],
                      data[year==2018 & product == "Other","units.pp.ontrade"],
                      data[year==2019 & product == "Other","units.pp.ontrade"]
)
imp.values.litres <- c(data[year==2015 & product == "Other","litres.pp.ontrade"],
                       data[year==2016 & product == "Other","litres.pp.ontrade"],
                       data[year==2017 & product == "Other","litres.pp.ontrade"],
                       data[year==2018 & product == "Other","litres.pp.ontrade"],
                       data[year==2019 & product == "Other","litres.pp.ontrade"]
)

library(data.table)

data <- data.table(data)

data[year==2015 & product == "RTDs",units.pp.ontrade :=  imp.values.units[1]]
data[year==2016 & product == "RTDs",units.pp.ontrade :=  imp.values.units[2]]
data[year==2017 & product == "RTDs",units.pp.ontrade :=  imp.values.units[3]]
data[year==2018 & product == "RTDs",units.pp.ontrade :=  imp.values.units[4]]
data[year==2019 & product == "RTDs",units.pp.ontrade :=  imp.values.units[5]]

data[year==2015 & product == "RTDs",litres.pp.ontrade :=  imp.values.litres[1]]
data[year==2016 & product == "RTDs",litres.pp.ontrade :=  imp.values.litres[2]]
data[year==2017 & product == "RTDs",litres.pp.ontrade :=  imp.values.litres[3]]
data[year==2018 & product == "RTDs",litres.pp.ontrade :=  imp.values.litres[4]]
data[year==2019 & product == "RTDs",litres.pp.ontrade :=  imp.values.litres[5]]

data <- data %>%
  filter(year >= 2000) %>%
  filter(product != "Other")



data_mesas_englandwales <- merge(data,population) %>%
  dplyr::mutate(units.ontrade   = (units.pp.ontrade  *population) ) %>%
  dplyr::mutate(units.offtrade  = (units.pp.offtrade *population) ) %>%
  dplyr::mutate(litres.ontrade  = (litres.pp.ontrade *population) ) %>%
  dplyr::mutate(litres.offtrade = (litres.pp.offtrade*population) ) %>%
  dplyr::mutate(litres.pu.ontrade  = (litres.ontrade /units.ontrade) ) %>%
  dplyr::mutate(litres.pu.offtrade = (litres.offtrade/units.offtrade) ) %>%
  dplyr::mutate(cons.ontrade = price.ontrade*(litres.ontrade/litres.pu.ontrade) ) %>%
  dplyr::mutate(cons.offtrade = price.offtrade*(litres.offtrade/litres.pu.offtrade) ) %>%
  dplyr::mutate(country = "England & Wales")


rm(year,data,population,imp.values,litres.data,litres.offtrade,litres.ontrade,
   price.offtrade,price.ontrade,prices.data,product,units.data,units.pp.offtrade,units.pp.ontrade)

#### Scottish Data ------------------------


#####################################################
############ YEAR AND PRODUCT IDENTIFIERS ###########

year <- rep(seq(1994,2019,1),each=9)

product <- rep(c("Total","Spirits","RTDs","Fortified Wines","Wine","Other","Cider","Perry","Beer"),2019-1994+1)


#####################################################
######## TOTAL CONSUMED IN 000s OF LITRES ###########

# consumption data - volume of pure alcohol (1000L). (on trade)

litres.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                          sheet = "Scotland data",
                          range = "C5:AB13",
                          col_names = FALSE)

litres.data <- as.matrix(litres.data)
litres.data <- round(as.numeric(litres.data),2)

litres.ontrade <- as.vector(matrix(litres.data,ncol=1))

# consumption data - volume of pure alcohol (1000L). (off trade)

litres.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                          sheet = "Scotland data",
                          range = "AE5:BD13",
                          col_names = FALSE)

litres.data <- as.matrix(litres.data)
litres.data <- round(as.numeric(litres.data),2)

litres.offtrade <- as.vector(matrix(litres.data,ncol=1))

#####################################################
############ UNITS CONSUMED PER PERSON ##############

# consumption data - units per person. (on trade)

units.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "Scotland data",
                         range = "C31:AB39",
                         col_names = FALSE)

units.data <- as.matrix(units.data)
units.data <- round(as.numeric(units.data),2)

units.pp.ontrade <- as.vector(matrix(units.data,ncol=1))

# consumption data - units per person. (off trade)

units.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "Scotland data",
                         range = "AE31:BD39",
                         col_names = FALSE)

units.data <- as.matrix(units.data)
units.data <- round(as.numeric(units.data),2)

units.pp.offtrade <- as.vector(matrix(units.data,ncol=1))

#####################################################
############ PRICES PER UNIT ########################

# price data - average price per unit sold (on trade)

prices.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                          sheet = "Scotland data",
                          range = "C44:AB52",
                          col_names = FALSE)

prices.data <- as.matrix(prices.data)
prices.data <- round(as.numeric(prices.data),2)


price.ontrade <- as.vector(matrix(prices.data,ncol=1))

# price data - average price per unit sold (off trade)

prices.data <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                          sheet = "Scotland data",
                          range = "AE44:BD52",
                          col_names = FALSE)

prices.data <- as.matrix(prices.data)
prices.data <- round(as.numeric(prices.data),2)

price.offtrade <- as.vector(matrix(prices.data,ncol=1))



#####################################################
############ POPULATION DATA ########################

population <- read_excel(path = paste0("data-raw/","mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "Population data",
                         range = "B5:C26",
                         col_names = FALSE)

population <- data.frame(population) %>%
  rename(year = ...1,
         population = ...2) %>%
  filter(year >= 2000)

#####################################################
######### COMBINE DATA INTO ONE FRAME ###############

data <- data.frame(year,product,
                   litres.ontrade,litres.offtrade,
                   units.pp.ontrade,units.pp.offtrade,
                   price.ontrade,price.offtrade)

## from 2015 impute RTD units per person in the on-trade as equal to the entire "other category"
## (RTD is merged into other with fortified wines and perry from this year onwards. Assume these other 2 = 0)

imp.values <- c(data[year==2015 & product == "Other","units.pp.ontrade"],
                data[year==2016 & product == "Other","units.pp.ontrade"],
                data[year==2017 & product == "Other","units.pp.ontrade"],
                data[year==2018 & product == "Other","units.pp.ontrade"],
                data[year==2019 & product == "Other","units.pp.ontrade"]
)

library(data.table)

data <- data.table(data)

data[year==2015 & product == "RTDs",units.pp.ontrade :=  imp.values[1]]
data[year==2016 & product == "RTDs",units.pp.ontrade :=  imp.values[2]]
data[year==2017 & product == "RTDs",units.pp.ontrade :=  imp.values[3]]
data[year==2018 & product == "RTDs",units.pp.ontrade :=  imp.values[4]]
data[year==2019 & product == "RTDs",units.pp.ontrade :=  imp.values[5]]

data <- data %>%
  filter(year >= 2000) %>%
  filter(product != "Other")



data_mesas_scotland <- merge(data,population) %>%
  mutate(units.ontrade = (units.pp.ontrade*population) ) %>%
  mutate(units.offtrade = (units.pp.offtrade*population) ) %>%
  select(year,product,units.ontrade,units.offtrade,price.ontrade,price.offtrade)

data_mesas_scotland$country <- "Scotland"

rm(year,data,population,imp.values,litres.data,litres.offtrade,litres.ontrade,
   price.offtrade,price.ontrade,prices.data,product,units.data,units.pp.offtrade,units.pp.ontrade)


####### COMBINE ENGLAND AND WALES WITH SCOTLAND

alcohol_data <- rbind(data_mesas_englandwales,data_mesas_scotland)

####### extract 2010 and 2019 prices for real-terms calculations

real.2010 <- alcohol_data %>%
  filter(year == 2010) %>%
  rename(price.ontrade_2010 = price.ontrade,
         price.offtrade_2010 = price.offtrade) %>%
  select(product,country,price.ontrade_2010,price.offtrade_2010)

real.2019 <- alcohol_data %>%
  filter(year == 2019) %>%
  rename(price.ontrade_2019 = price.ontrade,
         price.offtrade_2019 = price.offtrade) %>%
  select(product,country,price.ontrade_2019,price.offtrade_2019)

alcohol_data <- merge(alcohol_data,real.2010,
                      by=c("country","product"),
                      all=TRUE,
                      sort=TRUE)

alcohol_data <- merge(alcohol_data,real.2019,
                      by=c("country","product"),
                      all=TRUE,
                      sort=TRUE)

####### calculate consumption figures in nominal and real terms

alcohol_data <- alcohol_data %>%
  mutate(cons.on.nom =  (price.ontrade*units.ontrade)  /1000000) %>%
  mutate(cons.off.nom = (price.offtrade*units.offtrade)/1000000) %>%
  mutate(cons.on.2010 =  (price.ontrade_2010*units.ontrade)  /1000000) %>%
  mutate(cons.off.2010 = (price.offtrade_2010*units.offtrade)/1000000) %>%
  mutate(cons.on.2019 =  (price.ontrade_2019*units.ontrade)  /1000000) %>%
  mutate(cons.off.2019 = (price.offtrade_2019*units.offtrade)/1000000) %>%
  select(-c(price.ontrade_2010,price.ontrade_2019,price.offtrade_2010,price.offtrade_2019))


rm(real.2010,real.2019)

usethis::use_data(alcohol_data,overwrite=TRUE)
