library(readxl)
library(dplyr)

####### POPULATION DATA

population  <- read_excel("data-raw/mesas-monitoring-report-2020-alcohol-sales-price-and-affordability.xlsx",
                          sheet = "Population data",
                          range = "B5:D26",
                          col_names = FALSE)

population <- population %>%
  mutate(population = ...2 + ...3) %>%
  rename(year = ...1) %>%
  filter(year >= 2000) %>%
  select(year,population)

####### CONSUMPTION TRENDS
### note this is UK consumption, other data is just GB so will need to adjust
### (for now a quick google search says in 2017 NI was 2.1% of UK GDP, adjust consumption
### in line with this)

consumption <- read_excel("data-raw/consumertrendsq12020cvmsa.xls",
                          sheet = "02KS",
                          range = "A13:G35",
                          col_names = FALSE)

consumption <- consumption[,c(1,7)] %>%
  rename(year = ...1,
         expenditure = ...7) %>%
  mutate(expenditure = expenditure*1000000) %>%  # convert from millions into pound units
  mutate(expenditure.gb = expenditure*0.98) %>%
  filter(year >= 2000) %>%
  select(year,expenditure.gb)


####### SMOKING HABITS

### prevalance
habits.prevalance <- read_excel("data-raw/adultsmokinghabitsingreatbritain2019final.xls",
                                sheet = "Table 1",
                                range = "A27:U46",
                                col_names = FALSE)

habits.prevalance <- habits.prevalance[,c(1,21)] %>%
  rename(year = ...1) %>%
  mutate(prevalance = ...21/100) %>%
  select(year,prevalance)
habits.prevalance$year <- as.numeric(substr(habits.prevalance$year,1,4))

### number of cigarettes smoked
habits.ncigs <- read_excel("data-raw/adultsmokinghabitsingreatbritain2019final.xls",
                                sheet = "Table 4",
                                range = "A27:U48",
                                col_names = FALSE)

habits.ncigs <- habits.ncigs[-c(19,20),c(1,21)] %>%
  rename(year = ...1,
         ncigs = ...21)
habits.ncigs$year <- as.numeric(substr(habits.ncigs$year,1,4))

### RYO vs FM

habits.type <- read_excel("data-raw/adultsmokinghabitsingreatbritain2019final.xls",
                           sheet = "Table 5",
                           range = "A11:O16",
                           col_names = FALSE)

habits.type <- habits.type %>%
  rename(year = ...1) %>%
  mutate(ryo.prop = (...13+...15)/100) %>%
  mutate(fm.prop = (...12+...14)/100) %>%
  select(year,ryo.prop,fm.prop)

### HMRC quantity data

hmrc <- read_excel("data-raw/hmrc tobacco bulletin.xlsx",
                   range = "B20:L24",
                   col_names = FALSE)

hmrc <- hmrc %>%
  rename(year = ...1,
         FM.ncigs = ...6) %>%
  mutate(FM.ncigs = FM.ncigs*1000000) %>% # convert FM from millions of sticks, to sticks
  mutate(...9 = ...9*1000000) %>% # convert RYO from 1000kg to grams of tobacco
  mutate(RYOpack20 = ...9/12.5) %>% # convert RYO from grams into packs of 20 FM (pack of 20 = 12.5)
  mutate(RYO.ncigs = RYOpack20/20) %>% # convert RYO from packs to sticks
  select(year,FM.ncigs,RYO.ncigs)


######## MERGE into one data frame

merge1 <- merge(population,consumption,by="year")
merge2 <- merge(merge1,habits.prevalance,by="year")
merge3 <- merge(merge2,habits.ncigs,by="year")
merge4 <- merge(merge3,habits.type,by="year",all = TRUE)
merge5 <- merge(merge4,hmrc,by="year",all = TRUE)

rm(consumption,habits.prevalance,habits.ncigs,habits.type,population,merge1,merge2,merge3,merge4)

################ PRICE - divide total consumption by number of cigarettes

####### Total Cigarettes per year = average smoked per year * number of smokers
#######                           = (average daily*365) * (population*prevalance)

## (doesn't seem to produce sensible prices)

#data <- merge4 %>%
#  mutate(ncigs.total = ncigs*population*prevalance*365) %>%
#  mutate(cig.price = consumption/ncigs.total)



### Use HMRC data for total cigarettes consumed - 12.5g = one 20 pack of cigarettes based on
# Wood DM, Mould MG, Ong SB, Baker EH. "Pack year" smoking histories: what about patients who use loose tobacco?.
# Tob Control. 2005;14(2):141-142. doi:10.1136/tc.2004.009977


############################
###### CONSTRUCT PRICE DATA


### Price data: https://www.statista.com/statistics/414973/cigarette-prices-in-the-united-kingdom/ RRP of a pack of 20 (need to improve on this - better source)
### Price index https://www.statista.com/statistics/285163/tobacco-consumer-price-index-cpi-annually-in-the-united-kingdom-uk/

year <- seq(2003,2019,1)
price <- c(NA,NA,4.82,5.05,5.33,5.44,5.67,6.13,6.63,7.09,7.72,8.23,9.16,9.40,9.91,NA,NA)

# use CPI price index to fill in missing
price[1]  <- price[13]*0.488
price[2]  <- price[13]*0.506
price[16] <- price[13]*1.191
price[17] <- price[13]*1.257

price <- round(price,2)

price.per.cig <- price/20

prices.data <- data.frame(year,price,price.per.cig)

############################################
####### Merge into one data frame ##########

tobacco <- merge(merge5,prices.data,by="year",all=TRUE)

tobacco_data <- tobacco %>%
  select(-c(ryo.prop,RYO.ncigs,fm.prop,FM.ncigs)) %>%
  rename(expenditure = expenditure.gb) %>%
  mutate(consumption = expenditure/price.per.cig)

usethis::use_data(tobacco_data,overwrite = TRUE)
