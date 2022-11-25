library(readxl)
library(data.table)
library(magrittr)

### LFS employment data

devtools::install_git(
  "https://gitlab.com/stapm/r-packages/lfsclean.git",
  credentials = git2r::cred_user_pass("djmorris1989", getPass::getPass()),
  ref = "v1.0.0",
  build_vignettes = TRUE
)

library(lfsclean)


## construct 4-digit employment by industry-year from the Labour Force Survey

root <- "X:/"
file <- "ScHARR/PR_SPECTRUM/General/Data/LFS/tab/"
keep_vars <- c("year","quarter","pwt","d_country","l_lmstatus","l_full_time","l_sic2007_4dig")

data <- lfsclean::lfsclean(root = root,
                           file = file,
                           year = 2010:2020,
                           ages = 16:89,
                           keep_vars = keep_vars,
                           complete_vars = NULL,
                           deflator = "cpih")

##########################################################################
## restrict to all employed/self-employed with complete information on
## full time status and industry

data <- data[l_lmstatus=="employed" | l_lmstatus=="self employed" ,]
data <- data[!is.na(l_full_time) ,]
data <- data[!is.na(l_sic2007_4dig) ,]

########################################################
## calculate fte employment by industry and quarter, for the UK
## and separately for each constituent country

data[l_full_time == "full_time",fte_ := 1]
data[l_full_time == "part_time",fte_ := 0.5]

data_uk <- data[, .(fte   = sum(pwt*fte_),
                    total = sum(pwt )), by = c("year","time","l_sic2007_4dig")]

data_nire <- data[d_country == "Northern Ireland",
                  .(fte   = sum(pwt*fte_),
                    total = sum(pwt )), by = c("year","time","l_sic2007_4dig")]

data_scot <- data[d_country == "Scotland",
                  .(fte   = sum(pwt*fte_),
                    total = sum(pwt )), by = c("year","time","l_sic2007_4dig")]

data_eng <- data[d_country == "England",
                  .(fte   = sum(pwt*fte_),
                    total = sum(pwt )), by = c("year","time","l_sic2007_4dig")]

data_wales <- data[d_country == "Wales",
                 .(fte   = sum(pwt*fte_),
                   total = sum(pwt )), by = c("year","time","l_sic2007_4dig")]

## now average employment across quarters within years

data_uk <- data_uk[, .(fte = mean(fte), total = mean(total)), by = c("year","l_sic2007_4dig")]
data_eng <- data_eng[, .(fte_ = mean(fte), total_ = mean(total)), by = c("year","l_sic2007_4dig")]
data_nire <- data_nire[, .(fte_ = mean(fte), total_ = mean(total)), by = c("year","l_sic2007_4dig")]
data_scot <- data_scot[, .(fte_ = mean(fte), total_ = mean(total)), by = c("year","l_sic2007_4dig")]
data_wales <- data_wales[, .(fte_ = mean(fte), total_ = mean(total)), by = c("year","l_sic2007_4dig")]


setnames(data_uk, c("l_sic2007_4dig"), c("SIC_code"))
setnames(data_eng, c("l_sic2007_4dig"), c("SIC_code"))
setnames(data_nire, c("l_sic2007_4dig"), c("SIC_code"))
setnames(data_scot, c("l_sic2007_4dig"), c("SIC_code"))
setnames(data_wales, c("l_sic2007_4dig"), c("SIC_code"))

data_uk[, SIC_code := as.numeric(as.character(SIC_code))]
data_eng[, SIC_code := as.numeric(as.character(SIC_code))]
data_scot[, SIC_code := as.numeric(as.character(SIC_code))]
data_wales[, SIC_code := as.numeric(as.character(SIC_code))]
data_nire[, SIC_code := as.numeric(as.character(SIC_code))]

data_uk <- data_uk[order(year,SIC_code),]
data_eng <- data_eng[order(year,SIC_code),]
data_scot <- data_scot[order(year,SIC_code),]
data_wales <- data_wales[order(year,SIC_code),]
data_nire <- data_nire[order(year,SIC_code),]

saveRDS(data_uk, "data-raw/lfs_data_uk.rds")
saveRDS(data_eng, "data-raw/lfs_data_england.rds")
saveRDS(data_scot, "data-raw/lfs_data_scotland.rds")
saveRDS(data_wales, "data-raw/lfs_data_wales.rds")
saveRDS(data_nire, "data-raw/lfs_data_nireland.rds")

##################################################################
#### Create the employment tables for the different IO models ####

source("data-raw/raw data processing code/data processing - employment data LFS fai.R")
source("data-raw/raw data processing code/data processing - employment data LFS scotland.R")
source("data-raw/raw data processing code/data processing - employment data LFS northern ireland.R")

lfs_empl <- rbindlist(list(lfs_empl_fai, lfs_empl_scot, lfs_empl_ni))

usethis::use_data(lfs_empl, overwrite = TRUE)
