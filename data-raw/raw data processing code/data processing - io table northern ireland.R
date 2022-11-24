library(curl)
library(magrittr)
library(readxl)
library(data.table)
library(ggplot2)
library(htmltools)

sheet <- c("IXI,2016 ","IXI,2017")
year <- c(2016,2017)

for (s in 1:length(sheet)){

###########################################################
### Download the excel sheet from Scottish government #####

url <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/NI-2016-2017-Input-Output-tables-and-2017-multipliers.xlsx"
temp <- tempfile()
temp <- curl_download(url = url, destfile = temp, quiet = FALSE, mode = "wb")

### Read in the sector names
sector <- read_excel(temp,
                     sheet = sheet[s],
                     range = "A5:B67",
                     col_names = FALSE) %>% setDT

setnames(sector, names(sector), c("IOC","Sector"))

### Read in the flowtable
flowtable <- read_excel(temp,
                        sheet = sheet[s],
                        range = "C5:BM67",
                        col_names = FALSE) %>% setDT

##########################
### Read in other rows ###

### Read in the household consumption/demand and output rows
hhold.output <- read_excel(temp,
                           sheet = sheet[s],
                           range = "C73:BM73",
                           col_names = FALSE) %>% setDT


hhold.demand <- read_excel(temp,
                           sheet = sheet[s],
                           range = "BO5:BO67",
                           col_names = FALSE) %>% setDT

### Read in the final demand and total output

total.output <- read_excel(temp,
                           sheet = sheet[s],
                           range = "C76:BM76",
                           col_names = FALSE) %>% setDT

intermediate.demand <- read_excel(temp,
                                  sheet = sheet[s],
                                  range = "BN5:BN67",
                                  col_names = FALSE) %>% setDT

total.demand <- read_excel(temp,
                           sheet = sheet[s],
                           range = "BX5:BX67",
                           col_names = FALSE) %>% setDT

final.demand <- total.demand - intermediate.demand

govt.demand <- read_excel(temp,
                          sheet = sheet[s],
                          range = "BQ5:BQ67",
                          col_names = FALSE) %>% setDT

gva.taxes <- read_excel(temp,
                        sheet = sheet[s],
                        range = "C72:BM72",
                        col_names = FALSE) %>% setDT

gva.wages <- read_excel(temp,
                        sheet = sheet[s],
                        range = "C73:BM73",
                        col_names = FALSE) %>% setDT

gva.gos <- read_excel(temp,
                      sheet = sheet[s],
                      range = "C74:BM74",
                      col_names = FALSE) %>% setDT

gva.total <- read_excel(temp,
                        sheet = sheet[s],
                        range = "C75:BM75",
                        col_names = FALSE) %>% setDT

##########################################
#### Combine the data


flowtable <- as.matrix(flowtable)
total.demand <- as.vector(as.matrix(total.demand))
final.demand <- as.vector(as.matrix(final.demand))
govt.demand  <- as.vector(as.matrix(govt.demand))
hhold.demand <- as.vector(as.matrix(hhold.demand))
total.output <- as.vector(as.matrix(total.output))
hhold.output <- as.vector(as.matrix(hhold.output))

gva.taxes <- as.vector(as.matrix(gva.taxes))
gva.wages <- as.vector(as.matrix(gva.wages))
gva.gos   <- as.vector(as.matrix(gva.gos))
gva.total <- as.vector(as.matrix(gva.total))

iotable <- cbind(sector, flowtable,
                 hhold.demand, govt.demand, final.demand,
                 hhold.output, total.output, total.demand,
                 gva.taxes, gva.wages, gva.gos, gva.total)

setnames(iotable,
         old = names(iotable),
         new = c("IOC","Sector", paste0("sec",c(1:63)),
                 "hhold.demand", "govt.demand", "final.demand",
                 "hhold.output", "total.output", "total.demand",
                 "gva.taxes", "gva.wages", "gva.gos", "gva.total"))


iotable[, year := year[s] ]

if (s == 1){

  iotable_nire <- copy(iotable)
} else {

  iotable_nire <- rbindlist(list(iotable_nire, iotable))
}



}

usethis::use_data(iotable_nire, overwrite = TRUE)


