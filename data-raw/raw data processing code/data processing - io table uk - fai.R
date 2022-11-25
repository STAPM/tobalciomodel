library(curl)
library(magrittr)
library(readxl)
library(data.table)
library(ggplot2)
library(htmltools)

sheet <- "Sheet1"

###################################################
### Download the excel sheet from Strathclyde #####

url <- "https://pureportal.strath.ac.uk/files/86400329/2010_UK_Alcohol_consumption_disaggregated_IxI.xlsx"
temp <- tempfile()
temp <- curl_download(url = url, destfile = temp, quiet = FALSE, mode = "wb")



### Read in the sector names
sector <- read_excel(temp,
                     sheet = sheet,
                     range = "B6:C111",
                     col_names = FALSE)

setDT(sector)

setnames(sector, names(sector), c("IOC","Sector"))

### Read in the flowtable
flowtable <- read_excel(temp,
                        sheet = sheet,
                        range = "D6:DE111",
                        col_names = FALSE)
setDT(flowtable)


### Read in the household consumption/demand and output rows
hhold.output <- read_excel(temp,
                           sheet = sheet,
                           range = "D117:DE117",
                           col_names = FALSE)
setDT(hhold.output)


hhold.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "DG6:DG111",
                           col_names = FALSE)
setDT(hhold.demand)

### Read in the final demand and total output

total.output <- read_excel(temp,
                           sheet = sheet,
                           range = "D120:DE120",
                           col_names = FALSE)
setDT(total.output)


final.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "DS6:DS111",
                           col_names = FALSE)
setDT(final.demand)

total.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "DT6:DT111",
                           col_names = FALSE)
setDT(total.demand)

govt.demand <- read_excel(temp,
                          sheet = sheet,
                          range = "DI6:DI111",
                          col_names = FALSE)
setDT(govt.demand)

gva.taxes <- read_excel(temp,
                        sheet = sheet,
                        range = "D116:DE116",
                        col_names = FALSE)
setDT(gva.taxes)

gva.wages <- read_excel(temp,
                        sheet = sheet,
                        range = "D117:DE117",
                        col_names = FALSE)
setDT(gva.wages)

gva.gos <- read_excel(temp,
                      sheet = sheet,
                      range = "D118:DE118",
                      col_names = FALSE)
setDT(gva.gos)

gva.total <- read_excel(temp,
                        sheet = sheet,
                        range = "D119:DE119",
                        col_names = FALSE)
setDT(gva.total)


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

iotable_fai <- cbind(sector,flowtable,hhold.demand,govt.demand,final.demand,hhold.output,total.output,total.demand,
                               gva.taxes,gva.wages,gva.gos,gva.total)

setnames(iotable_fai, old = names(iotable_fai), new = c("IOC","Sector", paste0("sec",c(1:106)),
                                                                  "hhold.demand","govt.demand","final.demand",
                                                                  "hhold.output","total.output","total.demand",
                                                                  "gva.taxes","gva.wages","gva.gos","gva.total"))

iotable_fai[, year := 2010]

usethis::use_data(iotable_fai,overwrite=TRUE)
