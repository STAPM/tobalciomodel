library(readxl)

scenarios <- read_excel(paste0("data-raw/","io model scenarios",".xlsx"))

usethis::use_data(scenarios,overwrite=TRUE)
