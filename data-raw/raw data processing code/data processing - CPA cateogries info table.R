library(readxl)
library(data.table)

CPA <- readxl::read_excel("data-raw/supply and use 1997-2018.xlsx",
                          sheet = "Table 1 - Supply 2018",
                          range = "A3:B108",
                          col_names = TRUE)
setDT(CPA)

setnames(CPA, names(CPA), c("CPA_code","Product"))

usethis::use_data(CPA,overwrite = TRUE)
