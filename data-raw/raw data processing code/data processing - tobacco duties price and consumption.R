### The purpose of this code is to construct aggregate price and consumption data for
### tobacco products to use in aggregate analyses.

### Tobacco bulletin data: https://www.gov.uk/government/statistics/tobacco-bulletin
### Tobacco CPI: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7cb/mm23
### Cigarette 20-pack price: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/czmp

library(data.table)
library(readxl)
library(plyr)

##################################
#### INPUTS ######################

min_yr <- 2010
max_yr <- 2020

file <- "2021_JUL_Tobacco_Tables"

receipts <- "Table_1_receipts"
clear    <- "Table_2_clearances"

range_r <- "A55:D65"
range_c <- "A55:D65"

receipts_data <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                                    sheet = receipts,
                                    range = range_r,
                                    col_names = FALSE)

clear_data <- readxl::read_excel(paste0("data-raw/",file,".xlsx"),
                                 sheet = clear,
                                 range = range_c,
                                 col_names = FALSE)

price_and_cpi <- readxl::read_excel(paste0("data-raw/","tobacco_cpi_and_prices",".xlsx"),
                                    range = "A2:E13")

# average of price per 100g from supermarket websites December 2020
price_ryo_dec2020 <- 51.60

setDT(receipts_data)
setDT(clear_data)
setDT(price_and_cpi)

################################
#### FACTORY-MADE CIGS #########

r_fm <- receipts_data[,1:2]
c_fm <- clear_data[,1:2]

setnames(r_fm, names(r_fm), c("year","fm_duty_mn"))
setnames(c_fm, names(c_fm), c("year","fm_sticks_mn"))

fm <- merge(r_fm, c_fm)

# price data is for packs of 20, so convert the clearances from millions of sticks
# to numbers of packs of 20.

fm[, fm_20packs := (fm_sticks_mn*1000000)/20 ]

# merge in price data by year and calculate total expenditure in £mn

fm <- merge(fm, price_and_cpi[,c("year","price_20pack")], by = "year")

fm[, exp_mp := (price_20pack * fm_20packs )/1000000]

# calculate VAT on the expenditure at market prices and subtract taxes from expenditure
# to calculate expenditure at basic prices

fm[, vat := exp_mp*(0.2/1.2)]

fm[, exp_bp := exp_mp - vat - fm_duty_mn]

# implied basic price per pack
fm[, basic_price_20pack := round( (exp_bp*1000000)/fm_20packs , 2)]

# real terms expenditure - 2020 prices
bp2020 <- as.numeric(fm[year == 2020,"basic_price_20pack"])
p2020 <- as.numeric(fm[year == 2020,"price_20pack"])

fm[, basic_price_20pack_2020 := bp2020]
fm[, price_20pack_2020 := p2020]

fm[, exp_mp_2020 := (price_20pack_2020 * fm_20packs )/1000000]
fm[, exp_bp_2020 := (basic_price_20pack_2020 * fm_20packs )/1000000]


fm[, product := "FM_cigs"]

fm[, unit_define := "1 unit = Pack of 20 factory-made cigarettes"]

fm[, c("fm_sticks_mn") := NULL]
setnames(fm, names(fm), c("year","duty","units","price_per_unit","exp_mp","vat","exp_bp","basic_price_per_unit",
                          "basic_price_per_unit_2020","price_per_unit_2020","exp_mp_2020","exp_bp_2020","product","unit_define"))

fm <- fm[,c("year","product","unit_define","duty","vat","units",
            "price_per_unit","basic_price_per_unit","exp_mp","exp_bp",
            "price_per_unit_2020","basic_price_per_unit_2020","exp_mp_2020","exp_bp_2020")]

####################################
#### ROLL-YOUR-OWN TOBACCO #########

r_ryo <- receipts_data[,c(1,4)]
c_ryo <- clear_data[,c(1,4)]

setnames(r_ryo, names(r_ryo), c("year","ryo_duty_mn"))
setnames(c_ryo, names(c_ryo), c("year","ryo_1000kg"))

ryo <- merge(r_ryo, c_ryo)

# price data is for 100g bags, so convert the clearances from 1,000kgs
# to numbers of packs of 100g ( * 1000 to get kgs, * 1000 to get g (so * 1000000) then /100)

ryo[, units := (ryo_1000kg*1000000)/100 ]

# merge in tobacco CPI data by year and use the cpi_2020 deflator to adjust the 2020 price
# per 100g of hand-rolled tobacco to the price for the relevant year. then calculate total
# expenditures

ryo <- merge(ryo, price_and_cpi[,c("year","cpi_2020")], by = "year")
ryo[, price_per_unit := round(price_ryo_dec2020 * cpi_2020 , 2)]


ryo[, exp_mp := (price_per_unit * units )/1000000]

# calculate VAT on the expenditure at market prices and subtract taxes from expenditure
# to calculate expenditure at basic prices

ryo[, vat := exp_mp*(0.2/1.2)]

ryo[, exp_bp := exp_mp - vat - ryo_duty_mn]

# implied basic price per pack
ryo[, basic_price_per_unit := round( (exp_bp*1000000)/units , 2)]

# real terms expenditure - 2020 prices
bp2020 <- as.numeric(ryo[year == 2020,"basic_price_per_unit"])
p2020 <- as.numeric(ryo[year == 2020,"price_per_unit"])

ryo[, basic_price_per_unit_2020 := bp2020]
ryo[, price_per_unit_2020 := p2020]

ryo[, exp_mp_2020 := (price_per_unit_2020 * units )/1000000]
ryo[, exp_bp_2020 := (basic_price_per_unit_2020 * units )/1000000]


ryo[, product := "RYO_tob"]

ryo[, unit_define := "1 unit = 100g of hand-rolling tobacco"]

setnames(ryo, c("ryo_duty_mn"), c("duty"))

ryo <- ryo[,c("year","product","unit_define","duty","vat","units",
              "price_per_unit","basic_price_per_unit","exp_mp","exp_bp",
              "price_per_unit_2020","basic_price_per_unit_2020","exp_mp_2020","exp_bp_2020")]

############### Combine

tobacco_data <- rbindlist(list(fm, ryo))

rm(c_fm, c_ryo, clear_data, receipts_data, r_fm, r_ryo, price_and_cpi, fm, ryo,
   bp2020, p2020, price_ryo_dec2020, range_c, range_r, receipts, clear, file, max_yr, min_yr)

