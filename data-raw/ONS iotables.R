###### Read in IO tables from the ONS - produced tables consistent with
###### the same sectors 1997 - 2018. Look at how inter-industry relationships
###### have changed - especially since 2010.

filepath <- "data-raw/"

library(readxl)
library(data.table)

sector <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                     sheet = "Table 2 - Int Con 2018",
                     range = "B6:B110",
                     col_names = FALSE)

setDT(sector)
setnames(sector, old = names(sector), new = c("sector"))

## 2018

iotable2018 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2018",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2018 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2018",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2018 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2018",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2018 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2018",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2018 <- data.frame(hholdoutput2018)


totaloutput2018 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2018",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2018 <- data.frame(totaloutput2018)


totaldemand2018 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2018",
                              range = "V6:V110",
                              col_names = FALSE)

gva2018 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2018",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2018 <- data.frame(gva2018)

setDT(iotable2018)
setDT(hholddemand2018)
setDT(finaldemand2018)
setDT(hholdoutput2018)
setDT(totaloutput2018)
setDT(totaldemand2018)
setDT(gva2018)

setnames(iotable2018, old = names(iotable2018), new = paste0("sec",c(1:105)) )
setnames(hholddemand2018, old = names(hholddemand2018), new = c("hhold.demand"))
setnames(finaldemand2018, old = names(finaldemand2018), new = c("final.demand"))
setnames(hholdoutput2018, old = names(hholdoutput2018), new = c("hhold.output"))
setnames(totaloutput2018, old = names(totaloutput2018), new = c("total.output"))
setnames(totaldemand2018, old = names(totaldemand2018), new = c("total.demand"))
setnames(gva2018        , old = names(gva2018)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2018 <- cbind(sector,iotable2018,hholddemand2018,finaldemand2018,hholdoutput2018,totaloutput2018,totaldemand2018,gva2018)
data2018[, year := 2018]
rm(iotable2018,hholddemand2018,finaldemand2018,hholdoutput2018,totaloutput2018,totaldemand2018,gva2018)


## 2017

iotable2017 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2017",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2017 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2017",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2017 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2017",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2017 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2017",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2017 <- data.frame(hholdoutput2017)


totaloutput2017 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2017",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2017 <- data.frame(totaloutput2017)


totaldemand2017 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2017",
                              range = "V6:V110",
                              col_names = FALSE)

gva2017 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2017",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2017 <- data.frame(gva2017)

setDT(iotable2017)
setDT(hholddemand2017)
setDT(finaldemand2017)
setDT(hholdoutput2017)
setDT(totaloutput2017)
setDT(totaldemand2017)
setDT(gva2017)

setnames(iotable2017, old = names(iotable2017), new = paste0("sec",c(1:105)) )
setnames(hholddemand2017, old = names(hholddemand2017), new = c("hhold.demand"))
setnames(finaldemand2017, old = names(finaldemand2017), new = c("final.demand"))
setnames(hholdoutput2017, old = names(hholdoutput2017), new = c("hhold.output"))
setnames(totaloutput2017, old = names(totaloutput2017), new = c("total.output"))
setnames(totaldemand2017, old = names(totaldemand2017), new = c("total.demand"))
setnames(gva2017        , old = names(gva2017)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2017 <- cbind(sector,iotable2017,hholddemand2017,finaldemand2017,hholdoutput2017,totaloutput2017,totaldemand2017,gva2017)
data2017[, year := 2017]
rm(iotable2017,hholddemand2017,finaldemand2017,hholdoutput2017,totaloutput2017,totaldemand2017,gva2017)


## 2016

iotable2016 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2016",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2016 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2016",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2016 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2016",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2016 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2016",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2016 <- data.frame(hholdoutput2016)


totaloutput2016 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2016",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2016 <- data.frame(totaloutput2016)


totaldemand2016 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2016",
                              range = "V6:V110",
                              col_names = FALSE)

gva2016 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2016",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2016 <- data.frame(gva2016)

setDT(iotable2016)
setDT(hholddemand2016)
setDT(finaldemand2016)
setDT(hholdoutput2016)
setDT(totaloutput2016)
setDT(totaldemand2016)
setDT(gva2016)

setnames(iotable2016, old = names(iotable2016), new = paste0("sec",c(1:105)) )
setnames(hholddemand2016, old = names(hholddemand2016), new = c("hhold.demand"))
setnames(finaldemand2016, old = names(finaldemand2016), new = c("final.demand"))
setnames(hholdoutput2016, old = names(hholdoutput2016), new = c("hhold.output"))
setnames(totaloutput2016, old = names(totaloutput2016), new = c("total.output"))
setnames(totaldemand2016, old = names(totaldemand2016), new = c("total.demand"))
setnames(gva2016        , old = names(gva2016)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2016 <- cbind(sector,iotable2016,hholddemand2016,finaldemand2016,hholdoutput2016,totaloutput2016,totaldemand2016,gva2016)
data2016[, year := 2016]
rm(iotable2016,hholddemand2016,finaldemand2016,hholdoutput2016,totaloutput2016,totaldemand2016,gva2016)

## 2015

iotable2015 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2015",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2015 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2015",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2015 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2015",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2015 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2015",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2015 <- data.frame(hholdoutput2015)


totaloutput2015 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2015",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2015 <- data.frame(totaloutput2015)


totaldemand2015 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2015",
                              range = "V6:V110",
                              col_names = FALSE)

gva2015 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2015",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2015 <- data.frame(gva2015)

setDT(iotable2015)
setDT(hholddemand2015)
setDT(finaldemand2015)
setDT(hholdoutput2015)
setDT(totaloutput2015)
setDT(totaldemand2015)
setDT(gva2015)

setnames(iotable2015, old = names(iotable2015), new = paste0("sec",c(1:105)) )
setnames(hholddemand2015, old = names(hholddemand2015), new = c("hhold.demand"))
setnames(finaldemand2015, old = names(finaldemand2015), new = c("final.demand"))
setnames(hholdoutput2015, old = names(hholdoutput2015), new = c("hhold.output"))
setnames(totaloutput2015, old = names(totaloutput2015), new = c("total.output"))
setnames(totaldemand2015, old = names(totaldemand2015), new = c("total.demand"))
setnames(gva2015        , old = names(gva2015)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2015 <- cbind(sector,iotable2015,hholddemand2015,finaldemand2015,hholdoutput2015,totaloutput2015,totaldemand2015,gva2015)
data2015[, year := 2015]
rm(iotable2015,hholddemand2015,finaldemand2015,hholdoutput2015,totaloutput2015,totaldemand2015,gva2015)

## 2014

iotable2014 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2014",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2014 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2014",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2014 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2014",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2014 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2014",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2014 <- data.frame(hholdoutput2014)


totaloutput2014 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2014",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2014 <- data.frame(totaloutput2014)


totaldemand2014 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2014",
                              range = "V6:V110",
                              col_names = FALSE)

gva2014 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2014",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2014 <- data.frame(gva2014)

setDT(iotable2014)
setDT(hholddemand2014)
setDT(finaldemand2014)
setDT(hholdoutput2014)
setDT(totaloutput2014)
setDT(totaldemand2014)
setDT(gva2014)

setnames(iotable2014, old = names(iotable2014), new = paste0("sec",c(1:105)) )
setnames(hholddemand2014, old = names(hholddemand2014), new = c("hhold.demand"))
setnames(finaldemand2014, old = names(finaldemand2014), new = c("final.demand"))
setnames(hholdoutput2014, old = names(hholdoutput2014), new = c("hhold.output"))
setnames(totaloutput2014, old = names(totaloutput2014), new = c("total.output"))
setnames(totaldemand2014, old = names(totaldemand2014), new = c("total.demand"))
setnames(gva2014        , old = names(gva2014)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2014 <- cbind(sector,iotable2014,hholddemand2014,finaldemand2014,hholdoutput2014,totaloutput2014,totaldemand2014,gva2014)
data2014[, year := 2014]
rm(iotable2014,hholddemand2014,finaldemand2014,hholdoutput2014,totaloutput2014,totaldemand2014,gva2014)

## 2013

iotable2013 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2013",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2013 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2013",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2013 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2013",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2013 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2013",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2013 <- data.frame(hholdoutput2013)


totaloutput2013 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2013",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2013 <- data.frame(totaloutput2013)


totaldemand2013 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2013",
                              range = "V6:V110",
                              col_names = FALSE)

gva2013 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2013",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2013 <- data.frame(gva2013)

setDT(iotable2013)
setDT(hholddemand2013)
setDT(finaldemand2013)
setDT(hholdoutput2013)
setDT(totaloutput2013)
setDT(totaldemand2013)
setDT(gva2013)

setnames(iotable2013, old = names(iotable2013), new = paste0("sec",c(1:105)) )
setnames(hholddemand2013, old = names(hholddemand2013), new = c("hhold.demand"))
setnames(finaldemand2013, old = names(finaldemand2013), new = c("final.demand"))
setnames(hholdoutput2013, old = names(hholdoutput2013), new = c("hhold.output"))
setnames(totaloutput2013, old = names(totaloutput2013), new = c("total.output"))
setnames(totaldemand2013, old = names(totaldemand2013), new = c("total.demand"))
setnames(gva2013        , old = names(gva2013)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2013 <- cbind(sector,iotable2013,hholddemand2013,finaldemand2013,hholdoutput2013,totaloutput2013,totaldemand2013,gva2013)
data2013[, year := 2013]
rm(iotable2013,hholddemand2013,finaldemand2013,hholdoutput2013,totaloutput2013,totaldemand2013,gva2013)

## 2012

iotable2012 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2012",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2012 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2012",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2012 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2012",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2012 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2012",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2012 <- data.frame(hholdoutput2012)


totaloutput2012 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2012",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2012 <- data.frame(totaloutput2012)


totaldemand2012 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2012",
                              range = "V6:V110",
                              col_names = FALSE)

gva2012 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2012",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2012 <- data.frame(gva2012)

setDT(iotable2012)
setDT(hholddemand2012)
setDT(finaldemand2012)
setDT(hholdoutput2012)
setDT(totaloutput2012)
setDT(totaldemand2012)
setDT(gva2012)

setnames(iotable2012, old = names(iotable2012), new = paste0("sec",c(1:105)) )
setnames(hholddemand2012, old = names(hholddemand2012), new = c("hhold.demand"))
setnames(finaldemand2012, old = names(finaldemand2012), new = c("final.demand"))
setnames(hholdoutput2012, old = names(hholdoutput2012), new = c("hhold.output"))
setnames(totaloutput2012, old = names(totaloutput2012), new = c("total.output"))
setnames(totaldemand2012, old = names(totaldemand2012), new = c("total.demand"))
setnames(gva2012        , old = names(gva2012)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2012 <- cbind(sector,iotable2012,hholddemand2012,finaldemand2012,hholdoutput2012,totaloutput2012,totaldemand2012,gva2012)
data2012[, year := 2012]
rm(iotable2012,hholddemand2012,finaldemand2012,hholdoutput2012,totaloutput2012,totaldemand2012,gva2012)

## 2011

iotable2011 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2011",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2011 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2011",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2011 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2011",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2011 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2011",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2011 <- data.frame(hholdoutput2011)


totaloutput2011 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2011",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2011 <- data.frame(totaloutput2011)


totaldemand2011 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2011",
                              range = "V6:V110",
                              col_names = FALSE)

gva2011 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2011",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2011 <- data.frame(gva2011)

setDT(iotable2011)
setDT(hholddemand2011)
setDT(finaldemand2011)
setDT(hholdoutput2011)
setDT(totaloutput2011)
setDT(totaldemand2011)
setDT(gva2011)

setnames(iotable2011, old = names(iotable2011), new = paste0("sec",c(1:105)) )
setnames(hholddemand2011, old = names(hholddemand2011), new = c("hhold.demand"))
setnames(finaldemand2011, old = names(finaldemand2011), new = c("final.demand"))
setnames(hholdoutput2011, old = names(hholdoutput2011), new = c("hhold.output"))
setnames(totaloutput2011, old = names(totaloutput2011), new = c("total.output"))
setnames(totaldemand2011, old = names(totaldemand2011), new = c("total.demand"))
setnames(gva2011        , old = names(gva2011)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2011 <- cbind(sector,iotable2011,hholddemand2011,finaldemand2011,hholdoutput2011,totaloutput2011,totaldemand2011,gva2011)
data2011[, year := 2011]
rm(iotable2011,hholddemand2011,finaldemand2011,hholdoutput2011,totaloutput2011,totaldemand2011,gva2011)

## 2010

iotable2010 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2010",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2010 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2010",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2010 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2010",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2010 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2010",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2010 <- data.frame(hholdoutput2010)


totaloutput2010 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2010",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2010 <- data.frame(totaloutput2010)


totaldemand2010 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2010",
                              range = "V6:V110",
                              col_names = FALSE)

gva2010 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2010",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2010 <- data.frame(gva2010)

setDT(iotable2010)
setDT(hholddemand2010)
setDT(finaldemand2010)
setDT(hholdoutput2010)
setDT(totaloutput2010)
setDT(totaldemand2010)
setDT(gva2010)

setnames(iotable2010, old = names(iotable2010), new = paste0("sec",c(1:105)) )
setnames(hholddemand2010, old = names(hholddemand2010), new = c("hhold.demand"))
setnames(finaldemand2010, old = names(finaldemand2010), new = c("final.demand"))
setnames(hholdoutput2010, old = names(hholdoutput2010), new = c("hhold.output"))
setnames(totaloutput2010, old = names(totaloutput2010), new = c("total.output"))
setnames(totaldemand2010, old = names(totaldemand2010), new = c("total.demand"))
setnames(gva2010        , old = names(gva2010)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2010 <- cbind(sector,iotable2010,hholddemand2010,finaldemand2010,hholdoutput2010,totaloutput2010,totaldemand2010,gva2010)
data2010[, year := 2010]
rm(iotable2010,hholddemand2010,finaldemand2010,hholdoutput2010,totaloutput2010,totaldemand2010,gva2010)

## 2009

iotable2009 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2009",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2009 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2009",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2009 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2009",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2009 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2009",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2009 <- data.frame(hholdoutput2009)


totaloutput2009 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2009",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2009 <- data.frame(totaloutput2009)


totaldemand2009 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2009",
                              range = "V6:V110",
                              col_names = FALSE)

gva2009 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2009",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2009 <- data.frame(gva2009)

setDT(iotable2009)
setDT(hholddemand2009)
setDT(finaldemand2009)
setDT(hholdoutput2009)
setDT(totaloutput2009)
setDT(totaldemand2009)
setDT(gva2009)

setnames(iotable2009, old = names(iotable2009), new = paste0("sec",c(1:105)) )
setnames(hholddemand2009, old = names(hholddemand2009), new = c("hhold.demand"))
setnames(finaldemand2009, old = names(finaldemand2009), new = c("final.demand"))
setnames(hholdoutput2009, old = names(hholdoutput2009), new = c("hhold.output"))
setnames(totaloutput2009, old = names(totaloutput2009), new = c("total.output"))
setnames(totaldemand2009, old = names(totaldemand2009), new = c("total.demand"))
setnames(gva2009        , old = names(gva2009)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2009 <- cbind(sector,iotable2009,hholddemand2009,finaldemand2009,hholdoutput2009,totaloutput2009,totaldemand2009,gva2009)
data2009[, year := 2009]
rm(iotable2009,hholddemand2009,finaldemand2009,hholdoutput2009,totaloutput2009,totaldemand2009,gva2009)

## 2008

iotable2008 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2008",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2008 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2008",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2008 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2008",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2008 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2008",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2008 <- data.frame(hholdoutput2008)


totaloutput2008 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2008",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2008 <- data.frame(totaloutput2008)


totaldemand2008 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2008",
                              range = "V6:V110",
                              col_names = FALSE)

gva2008 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2008",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2008 <- data.frame(gva2008)

setDT(iotable2008)
setDT(hholddemand2008)
setDT(finaldemand2008)
setDT(hholdoutput2008)
setDT(totaloutput2008)
setDT(totaldemand2008)
setDT(gva2008)

setnames(iotable2008, old = names(iotable2008), new = paste0("sec",c(1:105)) )
setnames(hholddemand2008, old = names(hholddemand2008), new = c("hhold.demand"))
setnames(finaldemand2008, old = names(finaldemand2008), new = c("final.demand"))
setnames(hholdoutput2008, old = names(hholdoutput2008), new = c("hhold.output"))
setnames(totaloutput2008, old = names(totaloutput2008), new = c("total.output"))
setnames(totaldemand2008, old = names(totaldemand2008), new = c("total.demand"))
setnames(gva2008        , old = names(gva2008)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2008 <- cbind(sector,iotable2008,hholddemand2008,finaldemand2008,hholdoutput2008,totaloutput2008,totaldemand2008,gva2008)
data2008[, year := 2008]
rm(iotable2008,hholddemand2008,finaldemand2008,hholdoutput2008,totaloutput2008,totaldemand2008,gva2008)

## 2007

iotable2007 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2007",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2007 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2007",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2007 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2007",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2007 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2007",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2007 <- data.frame(hholdoutput2007)


totaloutput2007 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2007",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2007 <- data.frame(totaloutput2007)


totaldemand2007 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2007",
                              range = "V6:V110",
                              col_names = FALSE)

gva2007 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2007",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2007 <- data.frame(gva2007)

setDT(iotable2007)
setDT(hholddemand2007)
setDT(finaldemand2007)
setDT(hholdoutput2007)
setDT(totaloutput2007)
setDT(totaldemand2007)
setDT(gva2007)

setnames(iotable2007, old = names(iotable2007), new = paste0("sec",c(1:105)) )
setnames(hholddemand2007, old = names(hholddemand2007), new = c("hhold.demand"))
setnames(finaldemand2007, old = names(finaldemand2007), new = c("final.demand"))
setnames(hholdoutput2007, old = names(hholdoutput2007), new = c("hhold.output"))
setnames(totaloutput2007, old = names(totaloutput2007), new = c("total.output"))
setnames(totaldemand2007, old = names(totaldemand2007), new = c("total.demand"))
setnames(gva2007        , old = names(gva2007)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2007 <- cbind(sector,iotable2007,hholddemand2007,finaldemand2007,hholdoutput2007,totaloutput2007,totaldemand2007,gva2007)
data2007[, year := 2007]
rm(iotable2007,hholddemand2007,finaldemand2007,hholdoutput2007,totaloutput2007,totaldemand2007,gva2007)

## 2006

iotable2006 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2006",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2006 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2006",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2006 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2006",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2006 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2006",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2006 <- data.frame(hholdoutput2006)


totaloutput2006 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2006",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2006 <- data.frame(totaloutput2006)


totaldemand2006 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2006",
                              range = "V6:V110",
                              col_names = FALSE)

gva2006 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2006",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2006 <- data.frame(gva2006)

setDT(iotable2006)
setDT(hholddemand2006)
setDT(finaldemand2006)
setDT(hholdoutput2006)
setDT(totaloutput2006)
setDT(totaldemand2006)
setDT(gva2006)

setnames(iotable2006, old = names(iotable2006), new = paste0("sec",c(1:105)) )
setnames(hholddemand2006, old = names(hholddemand2006), new = c("hhold.demand"))
setnames(finaldemand2006, old = names(finaldemand2006), new = c("final.demand"))
setnames(hholdoutput2006, old = names(hholdoutput2006), new = c("hhold.output"))
setnames(totaloutput2006, old = names(totaloutput2006), new = c("total.output"))
setnames(totaldemand2006, old = names(totaldemand2006), new = c("total.demand"))
setnames(gva2006        , old = names(gva2006)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2006 <- cbind(sector,iotable2006,hholddemand2006,finaldemand2006,hholdoutput2006,totaloutput2006,totaldemand2006,gva2006)
data2006[, year := 2006]
rm(iotable2006,hholddemand2006,finaldemand2006,hholdoutput2006,totaloutput2006,totaldemand2006,gva2006)

## 2005

iotable2005 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2005",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2005 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2005",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2005 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2005",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2005 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2005",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2005 <- data.frame(hholdoutput2005)


totaloutput2005 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2005",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2005 <- data.frame(totaloutput2005)


totaldemand2005 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2005",
                              range = "V6:V110",
                              col_names = FALSE)

gva2005 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2005",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2005 <- data.frame(gva2005)

setDT(iotable2005)
setDT(hholddemand2005)
setDT(finaldemand2005)
setDT(hholdoutput2005)
setDT(totaloutput2005)
setDT(totaldemand2005)
setDT(gva2005)

setnames(iotable2005, old = names(iotable2005), new = paste0("sec",c(1:105)) )
setnames(hholddemand2005, old = names(hholddemand2005), new = c("hhold.demand"))
setnames(finaldemand2005, old = names(finaldemand2005), new = c("final.demand"))
setnames(hholdoutput2005, old = names(hholdoutput2005), new = c("hhold.output"))
setnames(totaloutput2005, old = names(totaloutput2005), new = c("total.output"))
setnames(totaldemand2005, old = names(totaldemand2005), new = c("total.demand"))
setnames(gva2005        , old = names(gva2005)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2005 <- cbind(sector,iotable2005,hholddemand2005,finaldemand2005,hholdoutput2005,totaloutput2005,totaldemand2005,gva2005)
data2005[, year := 2005]
rm(iotable2005,hholddemand2005,finaldemand2005,hholdoutput2005,totaloutput2005,totaldemand2005,gva2005)

## 2004

iotable2004 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2004",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2004 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2004",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2004 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2004",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2004 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2004",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2004 <- data.frame(hholdoutput2004)


totaloutput2004 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2004",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2004 <- data.frame(totaloutput2004)


totaldemand2004 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2004",
                              range = "V6:V110",
                              col_names = FALSE)

gva2004 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2004",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2004 <- data.frame(gva2004)

setDT(iotable2004)
setDT(hholddemand2004)
setDT(finaldemand2004)
setDT(hholdoutput2004)
setDT(totaloutput2004)
setDT(totaldemand2004)
setDT(gva2004)

setnames(iotable2004, old = names(iotable2004), new = paste0("sec",c(1:105)) )
setnames(hholddemand2004, old = names(hholddemand2004), new = c("hhold.demand"))
setnames(finaldemand2004, old = names(finaldemand2004), new = c("final.demand"))
setnames(hholdoutput2004, old = names(hholdoutput2004), new = c("hhold.output"))
setnames(totaloutput2004, old = names(totaloutput2004), new = c("total.output"))
setnames(totaldemand2004, old = names(totaldemand2004), new = c("total.demand"))
setnames(gva2004        , old = names(gva2004)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2004 <- cbind(sector,iotable2004,hholddemand2004,finaldemand2004,hholdoutput2004,totaloutput2004,totaldemand2004,gva2004)
data2004[, year := 2004]
rm(iotable2004,hholddemand2004,finaldemand2004,hholdoutput2004,totaloutput2004,totaldemand2004,gva2004)

## 2003

iotable2003 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2003",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2003 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2003",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2003 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2003",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2003 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2003",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2003 <- data.frame(hholdoutput2003)


totaloutput2003 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2003",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2003 <- data.frame(totaloutput2003)


totaldemand2003 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2003",
                              range = "V6:V110",
                              col_names = FALSE)

gva2003 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2003",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2003 <- data.frame(gva2003)

setDT(iotable2003)
setDT(hholddemand2003)
setDT(finaldemand2003)
setDT(hholdoutput2003)
setDT(totaloutput2003)
setDT(totaldemand2003)
setDT(gva2003)

setnames(iotable2003, old = names(iotable2003), new = paste0("sec",c(1:105)) )
setnames(hholddemand2003, old = names(hholddemand2003), new = c("hhold.demand"))
setnames(finaldemand2003, old = names(finaldemand2003), new = c("final.demand"))
setnames(hholdoutput2003, old = names(hholdoutput2003), new = c("hhold.output"))
setnames(totaloutput2003, old = names(totaloutput2003), new = c("total.output"))
setnames(totaldemand2003, old = names(totaldemand2003), new = c("total.demand"))
setnames(gva2003        , old = names(gva2003)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2003 <- cbind(sector,iotable2003,hholddemand2003,finaldemand2003,hholdoutput2003,totaloutput2003,totaldemand2003,gva2003)
data2003[, year := 2003]
rm(iotable2003,hholddemand2003,finaldemand2003,hholdoutput2003,totaloutput2003,totaldemand2003,gva2003)

## 2002

iotable2002 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2002",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2002 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2002",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2002 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2002",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2002 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2002",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2002 <- data.frame(hholdoutput2002)


totaloutput2002 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2002",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2002 <- data.frame(totaloutput2002)


totaldemand2002 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2002",
                              range = "V6:V110",
                              col_names = FALSE)

gva2002 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2002",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2002 <- data.frame(gva2002)

setDT(iotable2002)
setDT(hholddemand2002)
setDT(finaldemand2002)
setDT(hholdoutput2002)
setDT(totaloutput2002)
setDT(totaldemand2002)
setDT(gva2002)

setnames(iotable2002, old = names(iotable2002), new = paste0("sec",c(1:105)) )
setnames(hholddemand2002, old = names(hholddemand2002), new = c("hhold.demand"))
setnames(finaldemand2002, old = names(finaldemand2002), new = c("final.demand"))
setnames(hholdoutput2002, old = names(hholdoutput2002), new = c("hhold.output"))
setnames(totaloutput2002, old = names(totaloutput2002), new = c("total.output"))
setnames(totaldemand2002, old = names(totaldemand2002), new = c("total.demand"))
setnames(gva2002        , old = names(gva2002)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2002 <- cbind(sector,iotable2002,hholddemand2002,finaldemand2002,hholdoutput2002,totaloutput2002,totaldemand2002,gva2002)
data2002[, year := 2002]
rm(iotable2002,hholddemand2002,finaldemand2002,hholdoutput2002,totaloutput2002,totaldemand2002,gva2002)

## 2001

iotable2001 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2001",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2001 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2001",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2001 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2001",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2001 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2001",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2001 <- data.frame(hholdoutput2001)


totaloutput2001 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2001",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2001 <- data.frame(totaloutput2001)


totaldemand2001 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2001",
                              range = "V6:V110",
                              col_names = FALSE)

gva2001 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2001",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2001 <- data.frame(gva2001)

setDT(iotable2001)
setDT(hholddemand2001)
setDT(finaldemand2001)
setDT(hholdoutput2001)
setDT(totaloutput2001)
setDT(totaldemand2001)
setDT(gva2001)

setnames(iotable2001, old = names(iotable2001), new = paste0("sec",c(1:105)) )
setnames(hholddemand2001, old = names(hholddemand2001), new = c("hhold.demand"))
setnames(finaldemand2001, old = names(finaldemand2001), new = c("final.demand"))
setnames(hholdoutput2001, old = names(hholdoutput2001), new = c("hhold.output"))
setnames(totaloutput2001, old = names(totaloutput2001), new = c("total.output"))
setnames(totaldemand2001, old = names(totaldemand2001), new = c("total.demand"))
setnames(gva2001        , old = names(gva2001)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2001 <- cbind(sector,iotable2001,hholddemand2001,finaldemand2001,hholdoutput2001,totaloutput2001,totaldemand2001,gva2001)
data2001[, year := 2001]
rm(iotable2001,hholddemand2001,finaldemand2001,hholdoutput2001,totaloutput2001,totaldemand2001,gva2001)

## 2000

iotable2000 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 2000",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand2000 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2000",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand2000 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2000",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput2000 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2000",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput2000 <- data.frame(hholdoutput2000)


totaloutput2000 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 2000",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput2000 <- data.frame(totaloutput2000)


totaldemand2000 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 2000",
                              range = "V6:V110",
                              col_names = FALSE)

gva2000 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 2000",
                        range = "C112:DC115",
                        col_names = FALSE))
gva2000 <- data.frame(gva2000)

setDT(iotable2000)
setDT(hholddemand2000)
setDT(finaldemand2000)
setDT(hholdoutput2000)
setDT(totaloutput2000)
setDT(totaldemand2000)
setDT(gva2000)

setnames(iotable2000, old = names(iotable2000), new = paste0("sec",c(1:105)) )
setnames(hholddemand2000, old = names(hholddemand2000), new = c("hhold.demand"))
setnames(finaldemand2000, old = names(finaldemand2000), new = c("final.demand"))
setnames(hholdoutput2000, old = names(hholdoutput2000), new = c("hhold.output"))
setnames(totaloutput2000, old = names(totaloutput2000), new = c("total.output"))
setnames(totaldemand2000, old = names(totaldemand2000), new = c("total.demand"))
setnames(gva2000        , old = names(gva2000)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data2000 <- cbind(sector,iotable2000,hholddemand2000,finaldemand2000,hholdoutput2000,totaloutput2000,totaldemand2000,gva2000)
data2000[, year := 2000]
rm(iotable2000,hholddemand2000,finaldemand2000,hholdoutput2000,totaloutput2000,totaldemand2000,gva2000)

## 1999

iotable1999 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 1999",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand1999 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1999",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand1999 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1999",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput1999 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 1999",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput1999 <- data.frame(hholdoutput1999)


totaloutput1999 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 1999",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput1999 <- data.frame(totaloutput1999)


totaldemand1999 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1999",
                              range = "V6:V110",
                              col_names = FALSE)

gva1999 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 1999",
                        range = "C112:DC115",
                        col_names = FALSE))
gva1999 <- data.frame(gva1999)

setDT(iotable1999)
setDT(hholddemand1999)
setDT(finaldemand1999)
setDT(hholdoutput1999)
setDT(totaloutput1999)
setDT(totaldemand1999)
setDT(gva1999)

setnames(iotable1999, old = names(iotable1999), new = paste0("sec",c(1:105)) )
setnames(hholddemand1999, old = names(hholddemand1999), new = c("hhold.demand"))
setnames(finaldemand1999, old = names(finaldemand1999), new = c("final.demand"))
setnames(hholdoutput1999, old = names(hholdoutput1999), new = c("hhold.output"))
setnames(totaloutput1999, old = names(totaloutput1999), new = c("total.output"))
setnames(totaldemand1999, old = names(totaldemand1999), new = c("total.demand"))
setnames(gva1999        , old = names(gva1999)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data1999 <- cbind(sector,iotable1999,hholddemand1999,finaldemand1999,hholdoutput1999,totaloutput1999,totaldemand1999,gva1999)
data1999[, year := 1999]
rm(iotable1999,hholddemand1999,finaldemand1999,hholdoutput1999,totaloutput1999,totaldemand1999,gva1999)

## 1998

iotable1998 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 1998",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand1998 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1998",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand1998 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1998",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput1998 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 1998",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput1998 <- data.frame(hholdoutput1998)


totaloutput1998 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 1998",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput1998 <- data.frame(totaloutput1998)


totaldemand1998 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1998",
                              range = "V6:V110",
                              col_names = FALSE)

gva1998 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 1998",
                        range = "C112:DC115",
                        col_names = FALSE))
gva1998 <- data.frame(gva1998)

setDT(iotable1998)
setDT(hholddemand1998)
setDT(finaldemand1998)
setDT(hholdoutput1998)
setDT(totaloutput1998)
setDT(totaldemand1998)
setDT(gva1998)

setnames(iotable1998, old = names(iotable1998), new = paste0("sec",c(1:105)) )
setnames(hholddemand1998, old = names(hholddemand1998), new = c("hhold.demand"))
setnames(finaldemand1998, old = names(finaldemand1998), new = c("final.demand"))
setnames(hholdoutput1998, old = names(hholdoutput1998), new = c("hhold.output"))
setnames(totaloutput1998, old = names(totaloutput1998), new = c("total.output"))
setnames(totaldemand1998, old = names(totaldemand1998), new = c("total.demand"))
setnames(gva1998        , old = names(gva1998)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data1998 <- cbind(sector,iotable1998,hholddemand1998,finaldemand1998,hholdoutput1998,totaloutput1998,totaldemand1998,gva1998)
data1998[, year := 1998]
rm(iotable1998,hholddemand1998,finaldemand1998,hholdoutput1998,totaloutput1998,totaldemand1998,gva1998)

## 1997

iotable1997 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                          sheet = "Table 2 - Int Con 1997",
                          range = "C6:DC110",
                          col_names = FALSE)

hholddemand1997 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1997",
                              range = "C6:C110",
                              col_names = FALSE)

finaldemand1997 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1997",
                              range = "T6:T110",
                              col_names = FALSE)

hholdoutput1997 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 1997",
                                range = "C113:DC113",
                                col_names = FALSE))
hholdoutput1997 <- data.frame(hholdoutput1997)


totaloutput1997 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                                sheet = "Table 2 - Int Con 1997",
                                range = "C116:DC116",
                                col_names = FALSE))
totaloutput1997 <- data.frame(totaloutput1997)


totaldemand1997 <- read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                              sheet = "Table 2 - Final Demand 1997",
                              range = "V6:V110",
                              col_names = FALSE)

gva1997 <- t(read_excel(paste0(filepath,"1997 - 2018 ONS IO tables.xlsx"),
                        sheet = "Table 2 - Int Con 1997",
                        range = "C112:DC115",
                        col_names = FALSE))
gva1997 <- data.frame(gva1997)

setDT(iotable1997)
setDT(hholddemand1997)
setDT(finaldemand1997)
setDT(hholdoutput1997)
setDT(totaloutput1997)
setDT(totaldemand1997)
setDT(gva1997)

setnames(iotable1997, old = names(iotable1997), new = paste0("sec",c(1:105)) )
setnames(hholddemand1997, old = names(hholddemand1997), new = c("hhold.demand"))
setnames(finaldemand1997, old = names(finaldemand1997), new = c("final.demand"))
setnames(hholdoutput1997, old = names(hholdoutput1997), new = c("hhold.output"))
setnames(totaloutput1997, old = names(totaloutput1997), new = c("total.output"))
setnames(totaldemand1997, old = names(totaldemand1997), new = c("total.demand"))
setnames(gva1997        , old = names(gva1997)        , new = c("gva.taxes","gva.wages","gva.gos","gva.total"))


data1997 <- cbind(sector,iotable1997,hholddemand1997,finaldemand1997,hholdoutput1997,totaloutput1997,totaldemand1997,gva1997)
data1997[, year := 1997]
rm(iotable1997,hholddemand1997,finaldemand1997,hholdoutput1997,totaloutput1997,totaldemand1997,gva1997)


###### combine all data tables into one


iotables <- rbind(data1997,data1998,data1999,
                  data2000,data2001,data2002,data2003,data2004,
                  data2005,data2006,data2007,data2008,data2009,
                  data2010,data2011,data2012,data2013,data2014,
                  data2015,data2016,data2017,data2018)

rm(sector,        data1997,data1998,data1999,
   data2000,data2001,data2002,data2003,data2004,
   data2005,data2006,data2007,data2008,data2009,
   data2010,data2011,data2012,data2013,data2014,
   data2015,data2016,data2017,data2018)

setnames(iotables,c("sector"),c("name"))

data_iotables_ons <- iotables

usethis::use_data(data_iotables_ons,overwrite=TRUE)

