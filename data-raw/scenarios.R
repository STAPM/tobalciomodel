library(readxl)

scenarios <- read_excel(paste0("data-raw/","io model input parameters",".xlsx"),
                        sheet = "Parameters",
                        range = "A1:R101",
                        col_names = TRUE)

usethis::use_data(scenarios,overwrite=TRUE)
