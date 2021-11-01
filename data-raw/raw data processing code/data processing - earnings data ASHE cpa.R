library(readxl)
library(data.table)

## read in the SIC - CPA mapping sheet

map <- readxl::read_excel("data-raw/CPA SIC mapping.xlsx",
                          range = "A1:D613",
                          col_names = TRUE)
setDT(map)
map[, SIC_code := as.numeric(SIC_code)]

### ASHE earnings data

year <- 2020

earn <- readxl::read_excel(paste0("data-raw/SIC07 Industry (4) SIC2007 Table 16.7a   Annual pay - Gross ",2020,".xls"),
                           sheet = "All",
                           range = "A5:F997",
                           col_names = TRUE)
setDT(earn)

earn <- earn[,c(1:3,6)]

setnames(earn,
         names(earn),
         c("Industry","SIC_code","njobs","salary"))

earn[, SIC_code := as.numeric(SIC_code)]

### merge to the mapping file to isolate the 4-digit level industries
earn <- merge.data.table(earn,map[,"Industry"],by = "Industry", all.x = FALSE, all.y = TRUE, sort = FALSE)

### some duplicates - if a 3-digit industry has only one 4-digit component, they will have the same name

earn[, max := max(SIC_code), by = "Industry"]

earn <- earn[SIC_code == max,]

### this produces the 612 industries in the mapping file

earn <- earn[,-c("max")]

### set as numeric

earn[, salary := as.numeric(salary)]
earn[, njobs := as.numeric(njobs)]

### need employment to use as weights when mapping earnings to CPA but in ASHE data
### much of the njobs entries are missing, so match in FTE employment from the BRES/LFS data
### for the most recent year of data. Use LFS FTE totals as weights
source("data-raw/data processing - employment data LFS.R")

weights <- lfs_empl_sic[year == max(year),]

merge <- merge.data.table(earn, weights[,c("SIC_code","tot_fte")], by = "SIC_code")


merge_mapping <- merge.data.table(merge, map, by = c("SIC_code","Industry") )


### calculate weighted mean salary by CPA code and collapse data to CPA level

merge_mapping[, avg_salary := weighted.mean(salary,w = tot_fte, na.rm = TRUE), by = "CPA_code"]
merge_mapping <- unique(merge_mapping[,c("CPA_code","Product","avg_salary")])

####### -------- end generic code. from here, year-specific treatment of NA salary information ------- ######

### Where 4-digit SIC salary information is not available this is due to ONS suppressing unreliable information.
### If so, use the most detailed parent SIC category (i.e. 3 digit) possible as a proxy.

# For 2020, only an issue for 3 sectors

## 0 fte so produced weight of 0, but this is a 1:1 CPA SIC mapping so take this directly from SIC code 9700
merge_mapping[CPA_code == "CPA_T97", avg_salary := 12659]

## Owner-Occupiers Housing Services. Fill in with SIC "Renting and operating of own or leased real estate"
merge_mapping[CPA_code == "CPA_L68A", avg_salary := 31544]

## Remediation and other waste manaegemnt services. No reliable data even at the parent 2 digit SIC industry. So
## fill in with the mean for the whole of broad group E - Water supply, sewerage, and waste management/remediation.
merge_mapping[CPA_code == "CPA_E39", avg_salary := 37168]

## Mining support services No reliable data even at the parent 2 digit SIC industry. So
## fill in with the mean for the whole of broad group B - Mining and Quarrying.
merge_mapping[CPA_code == "CPA_B09", avg_salary := 55519]


ashe_earn_cpa <- copy(merge_mapping)

## rename some categories
merge_mapping[CPA_code == "CPA_B06 & B07", Product := "Crude Petroleum And Natural Gas & Metal Ores"]
merge_mapping[CPA_code == "CPA_C235_6"   , Product := "Manufacture of cement, lime, plaster and articles of concrete, cement and plaster"]
merge_mapping[CPA_code == "CPA_L68A"     , Product := "Imputed rents of owner-occupied dwellings"]
merge_mapping[CPA_code == "CPA_L68BXL683", Product := "Real estate services, excluding on a fee or contract basis and excluding imputed rent"]
merge_mapping[CPA_code == "CPA_L683"     , Product := "Real estate activities on a fee or contract basis"]





usethis::use_data(ashe_earn_cpa, overwrite = TRUE)


rm(list = ls())
