library(lfsclean)
library(readxl)
library(data.table)
library(magrittr)



### LFS employment data

## construct 4-digit employment by industry-year from the Labour Force Survey

root <- "D:/"
file <- "Datasets/Labour Force Survey/raw data/"

vars <- c("year","quarter","pwt","age","gender","lmstatus","full_time","sic2007_4dig")

data <- combine_years(list(
  lfs_clean_global(lfs_read_2010(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2011(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2012(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2013(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2014(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2015(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2016(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2017(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2018(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2019(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2020(root,file),keep_vars = vars)
)
)

## restrict to all employed/self-employed with complete information on
## full time status and industry

data <- data[lmstatus=="employed"|lmstatus=="self employed" ,]
data <- data[!is.na(full_time) ,]
data <- data[!is.na(sic2007_4dig) ,]

## calculate fte employment by industry and quarter

data[full_time == "full_time",fte_ := 1]
data[full_time == "part_time",fte_ := 0.5]

data[, fte   := sum(pwt*fte_), by = c("time","sic2007_4dig")]
data[, total := sum(pwt ),     by = c("time","sic2007_4dig")]

data <- unique(data[,c("time","year","sic2007_4dig","fte","total")])

## now average employment across quarters within years

data[, fte_   := mean(fte)  , by = c("year","sic2007_4dig")]
data[, total_ := mean(total), by = c("year","sic2007_4dig")]

data <- unique(data[,c("year","sic2007_4dig","fte_","total_")])

setnames(data,
         c("sic2007_4dig","fte_","total_"),
         c("SIC_code","fte","total"))

data[, SIC_code := as.numeric(as.character(SIC_code))]



#######################################################################
##### Map SIC Employment onto the CPA categories for the ONS tables ###

## read in the SIC - CPA mapping sheet

map <- readxl::read_excel("data-raw/CPA SIC mapping.xlsx",
                          range = "A1:D613",
                          col_names = TRUE)
setDT(map)
map[, SIC_code := as.numeric(SIC_code)]

## loop over years - merge in the mapping, assign 0s to 4-digit SIC
## industries not in the LFS (small sectors not captured in the sampling)
## and produce clean tables for SIC and CPA employment by year

for (y in 2010:2020) {

empl <- data[year == y,]

## map onto the CPA categories

merge <- merge.data.table(map, empl, by = "SIC_code", all.x = TRUE)

merge[is.na(total), total := 0]
merge[is.na(fte), fte := 0]

merge_sic <- merge[,c("SIC_code","Industry","total","fte")]
setnames(merge_sic, c("total","fte"), c("tot_emp","tot_fte"))

merge[, tot_emp := sum(total, na.rm=TRUE), by = "CPA_code"]
merge[, tot_fte := sum(fte, na.rm=TRUE)  , by = "CPA_code"]

merge <- unique(merge[,c("CPA_code","Product","tot_emp","tot_fte")])


merge[, year := y]
merge_sic[, year := y]

assign(paste("lfs_empl_sic",y,sep="."),merge_sic)
assign(paste("lfs_empl_cpa",y,sep="."),merge)
}

lfs_empl_cpa <- rbindlist(list(lfs_empl_cpa.2010,
                               lfs_empl_cpa.2011,
                               lfs_empl_cpa.2012,
                               lfs_empl_cpa.2013,
                               lfs_empl_cpa.2014,
                               lfs_empl_cpa.2015,
                               lfs_empl_cpa.2016,
                               lfs_empl_cpa.2017,
                               lfs_empl_cpa.2018,
                               lfs_empl_cpa.2019,
                               lfs_empl_cpa.2020))

lfs_empl_sic <- rbindlist(list(lfs_empl_sic.2010,
                               lfs_empl_sic.2011,
                               lfs_empl_sic.2012,
                               lfs_empl_sic.2013,
                               lfs_empl_sic.2014,
                               lfs_empl_sic.2015,
                               lfs_empl_sic.2016,
                               lfs_empl_sic.2017,
                               lfs_empl_sic.2018,
                               lfs_empl_sic.2019,
                               lfs_empl_sic.2020))

usethis::use_data(lfs_empl_cpa,overwrite = TRUE)

