library(readxl)
library(data.table)

## read in the SIC - CPA mapping sheet

map <- readxl::read_excel("data-raw/FAI SIC mapping.xlsx",
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



















