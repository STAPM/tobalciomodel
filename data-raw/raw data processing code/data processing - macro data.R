library(lfsclean)
library(readxl)
library(data.table)
library(magrittr)

############################
### LFS employment data ####

## construct 4-digit employment by industry-year from the Labour Force Survey

root <- "C:/"
file <- "Users/cm1djm/Documents/Datasets/Labour Force Survey/tab/"

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
  lfs_clean_global(lfs_read_2020(root,file),keep_vars = vars)   ))

## restrict to all employed/self-employed with complete information on
## full time status and industry

data <- data[lmstatus=="employed"|lmstatus=="self employed" ,]
data <- data[!is.na(full_time) ,]
data <- data[!is.na(sic2007_4dig) ,]

## calculate fte employment by quarter

data[full_time == "full_time",fte_ := 1]
data[full_time == "part_time",fte_ := 0.5]

data[, fte   := sum(pwt*fte_), by = c("time")]
data[, total := sum(pwt ),     by = c("time")]

data <- unique(data[,c("time", "year", "fte", "total")])

## now average employment across quarters within years

data[, fte_   := mean(fte)  , by = c("year")]
data[, total_ := mean(total), by = c("year")]

data <- unique(data[,c("year", "fte_", "total_")])

setnames(data,
         c("fte_","total_"),
         c("fte_empl","total_empl"))

empl <- copy(data)

#########################
### GROSS VALUE ADDED ###

gva <- readxl::read_excel("data-raw/series-271121-gva.xls",
                          range = "A8:B81")
setDT(gva)
setnames(gva, names(gva), c("year","gva"))

gva <- gva[year %in% 2010:2020,]
gva[, year := as.numeric(year) ]

macro_data <- merge(empl, gva, by = "year")

usethis::use_data(macro_data,overwrite = TRUE)
