library(readxl)
library(data.table)

sectors <- read_excel("data-raw/supply and use 1997-2018.xlsx",
                      sheet = paste0("Table 2 - Final Demand ",2018),
                      range = "A6:B110",
                      col_names = FALSE)

totals <- read_excel("data-raw/supply and use 1997-2018.xlsx",
                     sheet = paste0("Table 2 - Final Demand ",2018),
                     range = "E5:F110",
                     col_names = TRUE)

total_ctrl <- as.numeric(read_excel("data-raw/supply and use 1997-2018.xlsx",
                                    sheet = paste0("Table 2 - Final Demand ",2018),
                                    range = "E111:E111",
                                    col_names = FALSE))

total_local <- as.numeric(read_excel("data-raw/supply and use 1997-2018.xlsx",
                                     sheet = paste0("Table 2 - Final Demand ",2018),
                                     range = "F111:F111",
                                     col_names = FALSE))

setDT(sectors)
setDT(totals)

setnames(sectors,
         c("...1","...2"),
         c("CPA_code","Product"))

setnames(totals,
         names(totals),
         c("central","local"))

totals[, total := central + local]

totals[, total   := total/(total_ctrl + total_local)]
totals[, central := central/(total_ctrl)]
totals[, local   := local/(total_local)]


vectors_govt <- cbind(sectors,totals)

vectors_govt[, all_pubadmin   := c(rep(0,93),1,rep(0,11)) ]

vectors_govt[, all_education  := c(rep(0,94),1,rep(0,10)) ]

vectors_govt[, all_health     := c(rep(0,95),1,rep(0,9)) ]

vectors_govt[, all_socialwork := c(rep(0,96),1,rep(0,8)) ]

vectors_govt[, all_cultural   := c(rep(0,98),1,rep(0,6)) ]

usethis::use_data(vectors_govt, overwrite = TRUE)

