library(readxl)
library(data.table)

rm(list = ls())

path <- "data-raw/"


for (y in 1997:2018) {

supply <- readxl::read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                             sheet = paste0("Table 1 - Supply ",y),
                             range = "A3:K108",
                             col_names = TRUE)

setDT(supply)

supply <- supply[,-c(4:7)]

setnames(supply,
         names(supply),
         c("CPA_code","Product","output_bp","imports","margins","taxes","output_pp"))

### Read the Use table - product by product

iotable <- read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                      sheet = paste0("Table 2 - Int Con ",y),
                      range = "A5:DC110")
#iotable <- as.matrix(iotable)

### GVA figures and employment by sector, create a table of technical coefficients
### (input as proportion of total output)

gva.total <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                            sheet = paste0("Table 2 - Int Con ",y),
                                            range = "C115:DC115",
                                            col_names = FALSE)))
gva.taxes <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                            sheet = paste0("Table 2 - Int Con ",y),
                                            range = "C112:DC112",
                                            col_names = FALSE)))
gva.gos   <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                            sheet = paste0("Table 2 - Int Con ",y),
                                            range = "C114:DC114",
                                            col_names = FALSE)))
gva.wages <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                            sheet = paste0("Table 2 - Int Con ",y),
                                            range = "C113:DC113",
                                            col_names = FALSE)))

total.output <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                               sheet = paste0("Table 2 - Int Con ",y),
                                               range = "C116:DC116",
                                               col_names = FALSE) ))

hhold.output <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                               sheet = paste0("Table 2 - Int Con ",y),
                                               range = "C113:DC113",
                                               col_names = FALSE) ))



total.demand <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                               sheet = paste0("Table 2 - Final Demand ",y),
                                               range = "V6:V110",
                                               col_names = FALSE) ))

hhold.demand <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                               sheet = paste0("Table 2 - Final Demand ",y),
                                               range = "C6:C110",
                                               col_names = FALSE) ))

govt.demand <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                               sheet = paste0("Table 2 - Final Demand ",y),
                                               range = "E6:E110",
                                               col_names = FALSE) ))

final.demand <- as.vector(as.matrix(read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                                               sheet = paste0("Table 2 - Final Demand ",y),
                                               range = "T6:T110",
                                               col_names = FALSE) ))

gva <- data.table(supply$CPA_code, supply$Product,
                  hhold.demand, govt.demand, final.demand,
                  hhold.output, total.output, total.demand,
                  gva.taxes, gva.wages, gva.gos, gva.total)

setnames(gva, names(gva), c("CPA_code","Product","hhold.demand","govt.demand","final.demand","hhold.output",
                            "total.output","total.demand","gva.taxes","gva.wages","gva.gos","gva.total"))



supply <- data.table(supply)

iotable <- data.table(iotable)
setnames(iotable, "...1", "CPA_code")

gva <- data.table(gva)

data <- merge(iotable, gva, by = c("CPA_code", "Product"), sort = FALSE)
data <- merge(data, supply, by = c("CPA_code", "Product"), sort = FALSE)

### index year and combine years

data[, year := y]

if (y == 1997) {
iotable_ons <- data
} else if (y > 1997) {
iotable_ons <- rbind(iotable_ons, data)
}


}

usethis::use_data(iotable_ons, overwrite = TRUE)



