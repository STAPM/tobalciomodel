library(data.table)

######## mapping between SIC and CPA (ONS SUT tables) and IOC (FAI table)

## read in the SIC - FAI mapping sheet

map_fai <- readxl::read_excel("data-raw/FAI SIC mapping.xlsx",
                          range = "A1:D613",
                          col_names = TRUE)
setDT(map_fai)
map_fai[, SIC_code := as.numeric(SIC_code)]

## read in the SIC - FAI mapping sheet

map_cpa <- readxl::read_excel("data-raw/CPA SIC mapping.xlsx",
                          range = "A1:D613",
                          col_names = TRUE)
setDT(map_cpa)
map_cpa[, SIC_code := as.numeric(SIC_code)]

## merge

sic_cpa_fai_mapping <- merge(map_cpa, map_fai, by = c("Industry","SIC_code"), sort = F)

usethis::use_data(sic_cpa_fai_mapping,overwrite = TRUE)
