library(readxl)
library(data.table)
library(lfsclean)
library(magrittr)

root <- "C:/"
file <- "Users/damon/OneDrive/Documents/Datasets/Labour Force Survey/tab/" ### LFS data

filepath <- "C:/Users/damon/OneDrive/Documents/Datasets/Annual Survey of Hours and Earnings/by industry/"


###################################################
## read in the SIC - FAI sectors mapping sheet ####

map <- readxl::read_excel("data-raw/CPA SIC mapping.xlsx",
                          range = "A1:D613",
                          col_names = TRUE)
setDT(map)
map[, SIC_code := as.numeric(SIC_code)]

###################################
### read in LFS employment data ###

### need employment to use as weights when mapping earnings to CPA but in ASHE data
### much of the njobs entries are missing, so match in FTE employment from the BRES/LFS data
### for the most recent year of data. Use LFS FTE totals as weights

## construct 4-digit employment by industry-year from the Labour Force Survey


vars <- c("year","quarter","pwt","age","sex","lmstatus","full_time","sic2007_4dig")

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
  lfs_clean_global(lfs_read_2020(root,file),keep_vars = vars),
  lfs_clean_global(lfs_read_2021(root,file),keep_vars = vars)

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

## finally average across years

lfs_empl <- data[, .(fte = mean(fte, na.rm = TRUE),
                     total = mean(total,na.rm = TRUE)),
                 by = "SIC_code"]
rm(data, file, root, vars)

#############################
### ASHE earnings data ######

y <- 2021

earn <- readxl::read_excel(paste0(filepath,"SIC07 Industry (4) SIC2007 Table 16.7a   Annual pay - Gross ",y,".xls"),
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

#############################################################################
##### Deal with missing earnings - use mean earnings for the parent SIC #####

## read in identifiers for 4-digit, 3-digit, and 2-digit

sic <- readxl::read_excel(paste0("data-raw/4-digit SIC codes.xls"),
                          sheet = "All",
                          range = "A5:F997",
                          col_names = TRUE)
setDT(sic)
sic1 <- sic[Digit1 == 1,]
sic[, SIC_code := as.numeric(SIC_code)]
sic4 <- sic[Digit4 == 1,]
sic3 <- sic[Digit3 == 1,]
sic2 <- sic[Digit2 == 1,]

## generate 3-digit and 2-digit SIC code

sic4[, SIC_code_3dig := as.character(SIC_code)]
sic4[, SIC_code_3dig := substr(SIC_code_3dig,1,nchar(SIC_code_3dig)-1)]
sic4[, SIC_code_3dig := as.numeric(SIC_code_3dig)]

sic4[, SIC_code_2dig := as.character(SIC_code_3dig)]
sic4[, SIC_code_2dig := substr(SIC_code_2dig,1,nchar(SIC_code_2dig)-1)]
sic4[, SIC_code_2dig := as.numeric(SIC_code_2dig)]

sic4[SIC_code_2dig %in% c(1:3), SIC_code_1dig := "A"]
sic4[SIC_code_2dig %in% c(5:9), SIC_code_1dig := "B"]
sic4[SIC_code_2dig %in% c(10:33), SIC_code_1dig := "C"]
sic4[SIC_code_2dig %in% c(35), SIC_code_1dig := "D"]
sic4[SIC_code_2dig %in% c(36:39), SIC_code_1dig := "E"]
sic4[SIC_code_2dig %in% c(41:43), SIC_code_1dig := "F"]
sic4[SIC_code_2dig %in% c(45:47), SIC_code_1dig := "G"]
sic4[SIC_code_2dig %in% c(49:53), SIC_code_1dig := "H"]
sic4[SIC_code_2dig %in% c(55:56), SIC_code_1dig := "I"]
sic4[SIC_code_2dig %in% c(58:63), SIC_code_1dig := "J"]
sic4[SIC_code_2dig %in% c(64:66), SIC_code_1dig := "K"]
sic4[SIC_code_2dig %in% c(68), SIC_code_1dig := "L"]
sic4[SIC_code_2dig %in% c(69:75), SIC_code_1dig := "M"]
sic4[SIC_code_2dig %in% c(77:82), SIC_code_1dig := "N"]
sic4[SIC_code_2dig %in% c(84), SIC_code_1dig := "O"]
sic4[SIC_code_2dig %in% c(85), SIC_code_1dig := "P"]
sic4[SIC_code_2dig %in% c(86:88), SIC_code_1dig := "Q"]
sic4[SIC_code_2dig %in% c(90:93), SIC_code_1dig := "R"]
sic4[SIC_code_2dig %in% c(94:96), SIC_code_1dig := "S"]
sic4[SIC_code_2dig %in% c(97:99), SIC_code_1dig := "T"]

## merge to earnings data

earn4 <- merge(earn, sic4, by = c("Industry","SIC_code"), all.x = F, all.y = T, sort = F)
earn4[, c("Digit4","Digit3","Digit2","Digit1","njobs") := NULL]

earn3 <- merge(earn, sic3, by = c("Industry","SIC_code"), all.x = F, all.y = T, sort = F)
earn3[, c("Industry","Digit4","Digit3","Digit2","Digit1","njobs") := NULL]
setnames(earn3, c("SIC_code","salary"), c("SIC_code_3dig","salary_3dig"))

earn2 <- merge(earn, sic2, by = c("Industry","SIC_code"), all.x = F, all.y = T, sort = F)
earn2[, c("Industry","Digit4","Digit3","Digit2","Digit1","njobs") := NULL]
setnames(earn2, c("SIC_code","salary"), c("SIC_code_2dig","salary_2dig"))

earn = merge(earn4, earn3, by = "SIC_code_3dig", all.x = TRUE)
earn = merge(earn , earn2, by = "SIC_code_2dig", all.x = TRUE)

## if 4-digit SIC earnings are missing, impute using the next level up.

earn[is.na(salary), salary := salary_3dig ]
earn[is.na(salary), salary := salary_2dig ]
#earn[is.na(salary), salary := salary_1dig ]


### need employment to use as weights when mapping earnings to CPA but in ASHE data
### much of the njobs entries are missing, so match in FTE employment from the BRES/LFS data
### for the most recent year of data. Use LFS FTE totals as weights
#source("data-raw/data processing - employment data LFS.R")

weights <- lfs_empl[year == y,]

merge <- merge.data.table(earn, weights[,c("SIC_code","fte")], by = "SIC_code")


merge_mapping <- merge.data.table(merge, map, by = c("SIC_code","Industry") )


### calculate weighted mean salary by CPA code and collapse data to CPA level

merge_mapping[, avg_salary := weighted.mean(salary,w = fte, na.rm = TRUE), by = "CPA_code"]
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

#ashe_earn_cpa <- copy(merge_mapping)


## rename some categories
merge_mapping[CPA_code == "CPA_B06 & B07", Product := "Crude Petroleum And Natural Gas & Metal Ores"]
merge_mapping[CPA_code == "CPA_C235_6"   , Product := "Manufacture of cement, lime, plaster and articles of concrete, cement and plaster"]
merge_mapping[CPA_code == "CPA_L68A"     , Product := "Imputed rents of owner-occupied dwellings"]
merge_mapping[CPA_code == "CPA_L68BXL683", Product := "Real estate services, excluding on a fee or contract basis and excluding imputed rent"]
merge_mapping[CPA_code == "CPA_L683"     , Product := "Real estate activities on a fee or contract basis"]


ashe_earn_cpa <- copy(merge_mapping)

ashe_earn_cpa[ ,year := y]

usethis::use_data(ashe_earn_cpa, overwrite = TRUE)


rm(list = ls())
