### read in the flow table and other IO table elements into an R data file
library(readxl)
library(data.table)

path  <- "data-raw/"
file  <- "tobalciomodel.xlsx"
sheet <- "Elasticities - Meng"

data_elasticities_cross <- read_excel(path = paste0(path,file),
                                      sheet = sheet,
                                      range = "B1:K11",
                                      col_names = TRUE)
setDT(data_elasticities_cross)
data_elasticities_cross[, cross := "Yes"]

data_elasticities_nocross <- read_excel(path = paste0(path,file),
                                        sheet = sheet,
                                        range = "B15:K25",
                                        col_names = TRUE)

setDT(data_elasticities_nocross)
data_elasticities_nocross[, cross := "No"]

setnames(data_elasticities_nocross,
         c("...1","...2","...3","...4","...5","...6","...7","...8","...9","...10"),
         names(data_elasticities_cross[,1:10]))

data_elasticities <- rbind(data_elasticities_nocross,data_elasticities_cross)

usethis::use_data(data_elasticities,overwrite=TRUE)
