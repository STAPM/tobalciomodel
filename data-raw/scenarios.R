library(readxl)

scenarios <- read_excel(paste0("data-raw/","scenarios for io model",".xlsx"))

usethis::use_data(scenarios,overwrite=TRUE)
