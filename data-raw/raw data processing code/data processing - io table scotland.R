library(curl)
library(magrittr)
library(readxl)
library(data.table)
library(ggplot2)
library(htmltools)

sheet <- "IxI"

###########################################################
### Download the excel sheet from Scottish government #####

url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2019/08/input-output-latest/documents/all-tables-all-years/all-tables-all-years/govscot%3Adocument/SUT-98-19.xlsx"
temp <- tempfile()
temp <- curl_download(url = url, destfile = temp, quiet = FALSE, mode = "wb")

### Read in the sector names
sector <- read_excel(temp,
                     sheet = sheet,
                     range = "B8:C105",
                     col_names = FALSE) %>% setDT

setnames(sector, names(sector), c("IOC","Sector"))

######################
##### 1998 ###########
{
### Read in the flowtable
flowtable <- read_excel(temp,
                        sheet = sheet,
                        range = "D8:CW105",
                        col_names = FALSE) %>% setDT

##########################
### Read in other rows ###

### Read in the household consumption/demand and output rows
hhold.output <- read_excel(temp,
                           sheet = sheet,
                           range = "D112:CW112",
                           col_names = FALSE) %>% setDT


hhold.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "CY8:CY105",
                           col_names = FALSE) %>% setDT

### Read in the final demand and total output

total.output <- read_excel(temp,
                           sheet = sheet,
                           range = "D115:CW115",
                           col_names = FALSE) %>% setDT

final.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "DL8:DL105",
                           col_names = FALSE) %>% setDT

total.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "DM8:DM105",
                           col_names = FALSE) %>% setDT

govt.demand <- read_excel(temp,
                          sheet = sheet,
                          range = "DA8:DA105",
                          col_names = FALSE) %>% setDT

gva.taxes <- read_excel(temp,
                        sheet = sheet,
                        range = "D111:CW111",
                        col_names = FALSE) %>% setDT

gva.wages <- read_excel(temp,
                        sheet = sheet,
                        range = "D112:CW112",
                        col_names = FALSE) %>% setDT

gva.gos <- read_excel(temp,
                      sheet = sheet,
                      range = "D113:CW113",
                      col_names = FALSE) %>% setDT

gva.total <- read_excel(temp,
                        sheet = sheet,
                        range = "D114:CW114",
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

iotable_1998 <- cbind(sector, flowtable,
                      hhold.demand, govt.demand, final.demand,
                      hhold.output, total.output, total.demand,
                      gva.taxes, gva.wages, gva.gos, gva.total)

setnames(iotable_1998,
         old = names(iotable_1998),
         new = c("IOC","Sector", paste0("sec",c(1:98)),
                 "hhold.demand", "govt.demand", "final.demand",
                 "hhold.output", "total.output", "total.demand",
                 "gva.taxes", "gva.wages", "gva.gos", "gva.total"))

iotable_1998[, year := 1998]
}

######################
##### 2010 ###########
{
### Read in the flowtable
flowtable <- read_excel(temp,
                        sheet = sheet,
                        range = "D1304:CW1401",
                        col_names = FALSE) %>% setDT

##########################
### Read in other rows ###

### Read in the household consumption/demand and output rows
hhold.output <- read_excel(temp,
                           sheet = sheet,
                           range = "D1408:CW1408",
                           col_names = FALSE) %>% setDT


hhold.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "CY1304:CY1401",
                           col_names = FALSE) %>% setDT

### Read in the final demand and total output

total.output <- read_excel(temp,
                           sheet = sheet,
                           range = "D1411:CW1411",
                           col_names = FALSE) %>% setDT

final.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "DL1304:DL1401",
                           col_names = FALSE) %>% setDT

total.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "DM1304:DM1401",
                           col_names = FALSE) %>% setDT

govt.demand <- read_excel(temp,
                          sheet = sheet,
                          range = "DA1304:DA1401",
                          col_names = FALSE) %>% setDT

gva.taxes <- read_excel(temp,
                        sheet = sheet,
                        range = "D1407:CW1407",
                        col_names = FALSE) %>% setDT

gva.wages <- read_excel(temp,
                        sheet = sheet,
                        range = "D1408:CW1408",
                        col_names = FALSE) %>% setDT

gva.gos <- read_excel(temp,
                      sheet = sheet,
                      range = "D1409:CW1409",
                      col_names = FALSE) %>% setDT

gva.total <- read_excel(temp,
                        sheet = sheet,
                        range = "D1410:CW1410",
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

iotable_2010 <- cbind(sector, flowtable,
                      hhold.demand, govt.demand, final.demand,
                      hhold.output, total.output, total.demand,
                      gva.taxes, gva.wages, gva.gos, gva.total)

setnames(iotable_2010,
         old = names(iotable_2010),
         new = c("IOC","Sector", paste0("sec",c(1:98)),
                 "hhold.demand", "govt.demand", "final.demand",
                 "hhold.output", "total.output", "total.demand",
                 "gva.taxes", "gva.wages", "gva.gos", "gva.total"))

iotable_2010[, year := 2010]

}

######################
##### 2015 ###########
{
  ### Read in the flowtable
  flowtable <- read_excel(temp,
                          sheet = sheet,
                          range = "D1844:CW1941",
                          col_names = FALSE) %>% setDT

  ##########################
  ### Read in other rows ###

  ### Read in the household consumption/demand and output rows
  hhold.output <- read_excel(temp,
                             sheet = sheet,
                             range = "D1948:CW1948",
                             col_names = FALSE) %>% setDT


  hhold.demand <- read_excel(temp,
                             sheet = sheet,
                             range = "CY1844:CY1941",
                             col_names = FALSE) %>% setDT

  ### Read in the final demand and total output

  total.output <- read_excel(temp,
                             sheet = sheet,
                             range = "D1951:CW1951",
                             col_names = FALSE) %>% setDT

  final.demand <- read_excel(temp,
                             sheet = sheet,
                             range = "DL1844:DL1941",
                             col_names = FALSE) %>% setDT

  total.demand <- read_excel(temp,
                             sheet = sheet,
                             range = "DM1844:DM1941",
                             col_names = FALSE) %>% setDT

  govt.demand <- read_excel(temp,
                            sheet = sheet,
                            range = "DA1844:DA1941",
                            col_names = FALSE) %>% setDT

  gva.taxes <- read_excel(temp,
                          sheet = sheet,
                          range = "D1947:CW1947",
                          col_names = FALSE) %>% setDT

  gva.wages <- read_excel(temp,
                          sheet = sheet,
                          range = "D1948:CW1948",
                          col_names = FALSE) %>% setDT

  gva.gos <- read_excel(temp,
                        sheet = sheet,
                        range = "D1949:CW1949",
                        col_names = FALSE) %>% setDT

  gva.total <- read_excel(temp,
                          sheet = sheet,
                          range = "D1950:CW1950",
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

  iotable_2015 <- cbind(sector, flowtable,
                        hhold.demand, govt.demand, final.demand,
                        hhold.output, total.output, total.demand,
                        gva.taxes, gva.wages, gva.gos, gva.total)

  setnames(iotable_2015,
           old = names(iotable_2015),
           new = c("IOC","Sector", paste0("sec",c(1:98)),
                   "hhold.demand", "govt.demand", "final.demand",
                   "hhold.output", "total.output", "total.demand",
                   "gva.taxes", "gva.wages", "gva.gos", "gva.total"))

  iotable_2015[, year := 2015]

}

######################
##### 2019 ###########
{
  ### Read in the flowtable
  flowtable <- read_excel(temp,
                          sheet = sheet,
                          range = "D2276:CW2373",
                          col_names = FALSE) %>% setDT

  ##########################
  ### Read in other rows ###

  ### Read in the household consumption/demand and output rows
  hhold.output <- read_excel(temp,
                             sheet = sheet,
                             range = "D2380:CW2380",
                             col_names = FALSE) %>% setDT


  hhold.demand <- read_excel(temp,
                             sheet = sheet,
                             range = "CY2276:CY2373",
                             col_names = FALSE) %>% setDT

  ### Read in the final demand and total output

  total.output <- read_excel(temp,
                             sheet = sheet,
                             range = "D2383:CW2383",
                             col_names = FALSE) %>% setDT

  final.demand <- read_excel(temp,
                             sheet = sheet,
                             range = "DL2276:DL2373",
                             col_names = FALSE) %>% setDT

  total.demand <- read_excel(temp,
                             sheet = sheet,
                             range = "DM2276:DM2373",
                             col_names = FALSE) %>% setDT

  govt.demand <- read_excel(temp,
                            sheet = sheet,
                            range = "DA2276:DA2373",
                            col_names = FALSE) %>% setDT

  gva.taxes <- read_excel(temp,
                          sheet = sheet,
                          range = "D2379:CW2379",
                          col_names = FALSE) %>% setDT

  gva.wages <- read_excel(temp,
                          sheet = sheet,
                          range = "D2380:CW2380",
                          col_names = FALSE) %>% setDT

  gva.gos <- read_excel(temp,
                        sheet = sheet,
                        range = "D2381:CW2381",
                        col_names = FALSE) %>% setDT

  gva.total <- read_excel(temp,
                          sheet = sheet,
                          range = "D2382:CW2382",
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

  iotable_2019 <- cbind(sector, flowtable,
                        hhold.demand, govt.demand, final.demand,
                        hhold.output, total.output, total.demand,
                        gva.taxes, gva.wages, gva.gos, gva.total)

  setnames(iotable_2019,
           old = names(iotable_2019),
           new = c("IOC","Sector", paste0("sec",c(1:98)),
                   "hhold.demand", "govt.demand", "final.demand",
                   "hhold.output", "total.output", "total.demand",
                   "gva.taxes", "gva.wages", "gva.gos", "gva.total"))

  iotable_2019[, year := 2019]

}

################################################
#### COMBINE DATASETS FOR EACH YEAR INTO ONE ###

iotable_scot <- rbindlist(list(iotable_1998, iotable_2010, iotable_2015, iotable_2019))

usethis::use_data(iotable_scot, overwrite = TRUE)



