### read in the flow table and other IO table elements into an R data file
library(readxl)
library(data.table)

path  <- "data-raw/"
file  <- "2010_UK_Alcohol_consumption_disaggregated_IxI.xlsx"
sheet <- "Sheet1"

#path  <- "data-raw/"
#file  <- "tobalciomodel.xlsx"
#sheet <- "Input Output Table"

### Read in the sector names
sector <- read_excel(path = paste0(path,file),
                     sheet = sheet,
                     range = "C6:C111",
                     col_names = FALSE)

### Read in the flowtable
flowtable <- read_excel(path = paste0(path,file),
                        sheet = sheet,
                        range = "D6:DE111",
                        col_names = FALSE)

### Read in the household consumption/demand and output rows
hhold.output <- read_excel(path = paste0(path,file),
                           sheet = sheet,
                           range = "D117:DE117",
                           col_names = FALSE)


hhold.demand <- read_excel(path = paste0(path,file),
                           sheet = sheet,
                           range = "DG6:DG111",
                           col_names = FALSE)

### Read in the final demand and total output

total.output <- read_excel(path = paste0(path,file),
                           sheet = sheet,
                           range = "D120:DE120",
                           col_names = FALSE)


final.demand <- read_excel(path = paste0(path,file),
                           sheet = sheet,
                           range = "DS6:DS111",
                           col_names = FALSE)

total.demand <- read_excel(path = paste0(path,file),
                           sheet = sheet,
                           range = "DT6:DT111",
                           col_names = FALSE)

govt.demand <- read_excel(path = paste0(path,file),
                          sheet = sheet,
                          range = "DI6:DI111",
                          col_names = FALSE)

gva.taxes <- read_excel(path = paste0(path,file),
                        sheet = sheet,
                        range = "D116:DE116",
                        col_names = FALSE)

gva.wages <- read_excel(path = paste0(path,file),
                        sheet = sheet,
                        range = "D117:DE117",
                        col_names = FALSE)

gva.gos <- read_excel(path = paste0(path,file),
                      sheet = sheet,
                      range = "D118:DE118",
                      col_names = FALSE)

gva.total <- read_excel(path = paste0(path,file),
                        sheet = sheet,
                        range = "D119:DE119",
                        col_names = FALSE)


flowtable <- as.matrix(flowtable)
total.demand <- as.matrix(total.demand)
final.demand <- as.matrix(final.demand)
govt.demand  <- as.matrix(govt.demand)
hhold.demand <- as.matrix(hhold.demand)
total.output <- as.vector(as.matrix(total.output))
hhold.output <- as.vector(as.matrix(hhold.output))

gva.taxes <- as.vector(as.matrix(gva.taxes))
gva.wages <- as.vector(as.matrix(gva.wages))
gva.gos   <- as.vector(as.matrix(gva.gos))
gva.total <- as.vector(as.matrix(gva.total))

name <- as.vector(as.matrix(sector))

iotable_fai <- data.frame(name,flowtable,hhold.demand,govt.demand,final.demand,hhold.output,total.output,total.demand,
                               gva.taxes,gva.wages,gva.gos,gva.total)

setnames(iotable_fai, old = names(iotable_fai), new = c("name",paste0("sec",c(1:106)),
                                                                  "hhold.demand","govt.demand","final.demand",
                                                                  "hhold.output","total.output","total.demand",
                                                                  "gva.taxes","gva.wages","gva.gos","gva.total"))

usethis::use_data(iotable_fai,overwrite=TRUE)
