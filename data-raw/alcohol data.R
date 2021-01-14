### Read in the MESAS population, consumption, and price data

filepath <- "data-raw/"


library(readxl)
library(dplyr)
library(data.table)

#### England and Wales Data ------------------------

#### POPULATION DATA

population <- read_excel(path = paste0(filepath,"mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "Population data",
                         range = "B4:D26",
                         col_names = TRUE)

population <- population[,c("...1","England & Wales")]

setnames(population,
         old = c("...1","England & Wales"),
         new = c("year","population"))
setDT(population)
population$year <- as.character(population$year)

## TOTAL CONSUMED IN LITRES
# consumption data - volume of pure alcohol (L)


litres_on  <- read_excel(path = paste0(filepath,"mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "England & Wales data",
                         range = "B4:AB13",
                         col_names = TRUE)
setDT(litres_on)
setnames(litres_on, "(000 litres)", "product_name")

litres_on <- litres_on[ product_name != 'Total' & product_name != "Other"]
litres_on <- melt.data.table(litres_on,id.vars = "product_name",variable.name = "year")

litres_on[product_name == "Spirits", product := "on_spirits"]
litres_on[product_name == "RTDs", product := "on_rtds"]
litres_on[product_name == "Fortified Wines", product := "on_wine"]
litres_on[product_name == "Wine", product := "on_wine"]
litres_on[product_name == "Cider", product := "on_cider"]
litres_on[product_name == "Perry", product := "on_cider"]
litres_on[product_name == "Beer", product := "on_beer"]
litres_on[, trade := "on"]

litres_on$value <- as.numeric(litres_on$value)



litres_off <- read_excel(path = paste0(filepath,"mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                         sheet = "England & Wales data",
                         range = "AD4:BD13",
                         col_names = TRUE)

setDT(litres_off)
setnames(litres_off, "(000 litres)", "product_name")

litres_off <- litres_off[ product_name != 'Total' & product_name != "Other"]
litres_off <- melt.data.table(litres_off,id.vars = "product_name",variable.name = "year")

litres_off[product_name == "Spirits", product := "off_spirits"]
litres_off[product_name == "RTDs", product := "off_rtds"]
litres_off[product_name == "Fortified Wines", product := "off_wine"]
litres_off[product_name == "Wine", product := "off_wine"]
litres_off[product_name == "Cider", product := "off_cider"]
litres_off[product_name == "Perry", product := "off_cider"]
litres_off[product_name == "Beer", product := "off_beer"]
litres_off[, trade := "off"]

litres_off$value <- as.numeric(litres_off$value)


litres <- rbind(litres_on,litres_off)

litres[,value := round(value*1000)]

setnames(litres,
         old = c("value"),
         new = c("litres"))
rm(litres_on,litres_off)

## TOTAL CONSUMED IN UNITS PER ADULT
# consumption data - need to keep "other" here in on trade - these are RTDS after 2015


units_on <- read_excel(path = paste0(filepath,"mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                       sheet = "England & Wales data",
                       range = "B30:AB39",
                       col_names = TRUE)

setDT(units_on)
setnames(units_on, "Units per adult", "product_name")

units_on <- units_on[ product_name != 'Total' & product_name != "Other"]
units_on <- melt.data.table(units_on,id.vars = "product_name",variable.name = "year")

units_on[product_name == "Spirits", product := "on_spirits"]
units_on[product_name == "RTDs", product := "on_rtds"]
units_on[product_name == "Other", product := "on_rtds"]
units_on[product_name == "Fortified Wines", product := "on_wine"]
units_on[product_name == "Wine", product := "on_wine"]
units_on[product_name == "Cider", product := "on_cider"]
units_on[product_name == "Perry", product := "on_cider"]
units_on[product_name == "Beer", product := "on_beer"]
units_on[, trade := "on"]

units_on$value <- as.numeric(units_on$value)

units_off <- read_excel(path = paste0(filepath,"mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                        sheet = "England & Wales data",
                        range = "Ad30:BD39",
                        col_names = TRUE)

setDT(units_off)
setnames(units_off, "Units per adult", "product_name")

units_off <- units_off[ product_name != 'Total' & product_name != "Other"]
units_off <- melt.data.table(units_off,id.vars = "product_name",variable.name = "year")

units_off[product_name == "Spirits", product := "off_spirits"]
units_off[product_name == "RTDs", product := "off_rtds"]
units_off[product_name == "Fortified Wines", product := "off_wine"]
units_off[product_name == "Wine", product := "off_wine"]
units_off[product_name == "Cider", product := "off_cider"]
units_off[product_name == "Perry", product := "off_cider"]
units_off[product_name == "Beer", product := "off_beer"]
units_off[, trade := "off"]

units_off$value <- as.numeric(units_off$value)

units <- rbind(units_on,units_off)

setnames(units,
         old = c("value"),
         new = c("units_pp"))
rm(units_on,units_off)



## PRICE PER UNIT


price_on <- read_excel(path = paste0(filepath,"mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                       sheet = "England & Wales data",
                       range = "B43:AB52",
                       col_names = TRUE)

setDT(price_on)
setnames(price_on, "£ per unit of alcohol", "product_name")

price_on <- price_on[ product_name != 'Total' & product_name != "Other"]
price_on <- melt.data.table(price_on,id.vars = "product_name",variable.name = "year")

price_on[product_name == "Spirits", product := "on_spirits"]
price_on[product_name == "RTDs", product := "on_rtds"]
price_on[product_name == "Fortified Wines", product := "on_wine"]
price_on[product_name == "Wine", product := "on_wine"]
price_on[product_name == "Cider", product := "on_cider"]
price_on[product_name == "Perry", product := "on_cider"]
price_on[product_name == "Beer", product := "on_beer"]
price_on[, trade := "on"]

price_on$value <- as.numeric(price_on$value)



price_off <- read_excel(path = paste0(filepath,"mesas-monitoring-report-2020-alcohol-sales-price-and-affordability",".xlsx"),
                        sheet = "England & Wales data",
                        range = "AD43:BD52",
                        col_names = TRUE)

setDT(price_off)
setnames(price_off, "£ per unit of alcohol", "product_name")

price_off <- price_off[ product_name != 'Total' & product_name != "Other"]
price_off <- melt.data.table(price_off,id.vars = "product_name",variable.name = "year")

price_off[product_name == "Spirits", product := "off_spirits"]
price_off[product_name == "RTDs", product := "off_rtds"]
price_off[product_name == "Fortified Wines", product := "off_wine"]
price_off[product_name == "Wine", product := "off_wine"]
price_off[product_name == "Cider", product := "off_cider"]
price_off[product_name == "Perry", product := "off_cider"]
price_off[product_name == "Beer", product := "off_beer"]
price_off[, trade := "off"]

price_off$value <- as.numeric(price_off$value)

price <- rbind(price_on,price_off)

setnames(price,
         old = c("value"),
         new = c("price_pu"))
rm(price_on,price_off)


################################
### merge all data together ####


merge1 <- merge.data.table(litres,units, by = c("product_name","trade","year","product"),all = TRUE)
merge2 <- merge.data.table(merge1,price, by = c("product_name","trade","year","product"))
merge3 <- merge.data.table(merge2,population,by = "year",all.x = TRUE)

## total units

merge3[, units := units_pp*population]


## create weight from litres of consumption

merge3[, total := sum(litres), by = c("product","trade","year")]
merge3[, weight := litres/total]


## year as continuous

merge3[, year := as.numeric(year)]

##### collapse to 5-product level

data_mesas_englandwales <- merge3 %>%
  filter(year >= 2000) %>%
  group_by(product,year) %>%
  mutate(units_total = sum(units,na.rm = TRUE)) %>%
  mutate(litres_total = sum(total,na.rm = TRUE)) %>%
  mutate(price = weighted.mean(price_pu,w=weight)) %>%
  select(product,year,units_total,litres_total,price,population,trade) %>%
  distinct()

setDT(data_mesas_englandwales)


usethis::use_data(data_mesas_englandwales,overwrite=TRUE)
