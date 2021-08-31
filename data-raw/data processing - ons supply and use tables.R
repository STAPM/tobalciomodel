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
         c("code","Product","output_bp","imports","margins","taxes","output_pp"))

supply[, tax_prop := round(taxes/output_pp,3)]
supply[, scale_bp_to_pp := output_pp/output_bp]
supply[, scale_pp_to_bp := output_bp/output_pp]

### Read the Use table - product by product

iotable <- read_excel(paste0(path,"/supply and use 1997-2018.xlsx"),
                      sheet = paste0("Table 2 - Int Con ",y),
                      range = "B5:DC110")
iotable <- as.matrix(iotable)

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


gva <- matrix(c(supply$Product,
                hhold.demand,
                govt.demand,
                final.demand,
                hhold.output,
                total.output,
                total.demand,
                gva.taxes,
                gva.wages,
                gva.gos,
                gva.total),
              nrow = 105,
              byrow = FALSE,
              dimnames = list(NULL,
                              c("Product","hhold.demand","govt.demand","final.demand","hhold.output",
                                "total.output","total.demand","gva.taxes","gva.wages","gva.gos","gva.total")))


supply <- data.table(supply)
iotable <- data.table(iotable)
gva <- data.table(gva)

data <- merge(iotable, gva, by = "Product")
data <- merge(data, supply, by = "Product")

### index year and combine years

data[, year := y]

if (y == 1997) {
iotable_ons <- data
} else if (y > 1997) {
iotable_ons <- rbind(iotable_ons, data)
}


}

usethis::use_data(iotable_ons, overwrite = TRUE)



