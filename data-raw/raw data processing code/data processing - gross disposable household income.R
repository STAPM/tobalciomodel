library(curl)
library(magrittr)
library(readxl)
library(data.table)
library(ggplot2)
library(htmltools)

sheet <- "Table 1"

####################################################
### Download the excel sheet from ONS for GDHI #####

url <- "https://www.ons.gov.uk/file?uri=/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalgrossdisposablehouseholdincomegdhi/1997to2020/regionalgrossdisposablehouseholdincomeallitlregions.xls"
temp <- tempfile()
temp <- curl_download(url = url, destfile = temp, quiet = FALSE, mode = "wb")

### process GDHI by UK nation

data <- read_excel(temp,
                   sheet = sheet,
                   range = "A2:AA235",
                   col_names = TRUE) %>% setDT

setnames(data, c("Region name","ITL level"), c("country", "ITL"))

data <- data[ITL == "ITL1" | country %in% c("United Kingdom","England"),]

data <- data[country %in% c("United Kingdom","England","Scotland",
                            "Wales","Northern Ireland"), ]

data[, c("ITL","ITL code") := NULL]

### reshape into long form

gdhi <- melt(data, id.vars = "country", variable.name = "year", value.name = "gdhi")


usethis::use_data(gdhi, overwrite = TRUE)



