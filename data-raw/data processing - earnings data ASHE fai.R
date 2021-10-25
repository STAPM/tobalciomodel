library(readxl)
library(data.table)

## read in the SIC - CPA mapping sheet

map <- read.csv("data-raw/FAI SIC mapping.csv")
setDT(map)
setnames(map, names(map), c("Industry","SIC_code","IOC","Sector"))
map[, SIC_code := as.numeric(SIC_code)]

### ASHE earnings data

year <- 2020

earn <- readxl::read_excel(paste0("data-raw/SIC07 Industry (4) SIC2007 Table 16.7a   Annual pay - Gross ",2020,".xls"),
                           sheet = "All",
                           range = "A5:S997",
                           col_names = TRUE)
setDT(earn)

earn <- earn[,c(1:3,6,18,19)]

setnames(earn,
         names(earn),
         c("Industry","SIC_code","njobs","salary","Digit4","Digit3"))

earn[, SIC_code := as.numeric(SIC_code)]

earn <- earn[Digit4 == 1,]


### set as numeric

earn[, salary := as.numeric(salary)]
earn[, njobs := as.numeric(njobs)]

## merge to the FAI mapping

earn <- earn[,c("Industry","SIC_code","salary")]

earn <- merge(earn, map, by = c("SIC_code"))



### need employment to use as weights when mapping earnings to CPA but in ASHE data
### much of the njobs entries are missing, so match in FTE employment from the BRES/LFS data
### for the most recent year of data. Use LFS FTE totals as weights



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

lfs_empl <- data[, .(fte = mean(fte, na.rm = TRUE),
                     total = mean(total,na.rm = TRUE)),
                 by = "SIC_code"]


### merge employment and earnings data. collapse to FAI industry level.

data <- merge(earn, lfs_empl, by = c("SIC_code"), all.x = TRUE)

data <- data[, .(avg_salary = weighted.mean(salary, w = fte, na.rm=TRUE) ) ,
             by = c("IOC","Sector")]



fai_data <- tobalciomodel::lfs_empl_fai[year == 2010,c("IOC","Sector")]

final_data <- merge(x = fai_data,
                    y = data[,c("IOC","avg_salary")],
                    by = "IOC",
                    all = TRUE,
                    sort = FALSE)

# fill in alcohol disaggregated industries with the same wage as the aggregated sector.

sec1 <- as.numeric(final_data[IOC == 46, "avg_salary"])
sec2 <- as.numeric(final_data[IOC == 53, "avg_salary"])
sec3 <- as.numeric(final_data[IOC == 55, "avg_salary"])


final_data[61, avg_salary := sec1]
final_data[69, avg_salary := sec2]
final_data[71, avg_salary := sec3]

ashe_earn_fai <- copy(final_data)

#setnames(ashe_earn_fai, "Product", "Sector")

### save out the dataset

#################### 3 NAs:

# IOC = 09, Mining Support Service Activities
# IOC = 12, Manufacture of Tobacco Products
# IOC = 39, Remediation Activities And Other Waste Management Services

##### Replace with (from ONS) :

# CPA_B09, Mining support services = 55519.00
# CPA_C11.01-6 & C12, Manufacture of Alcohol and Tobacco Products = 34890.40
# CPA_E39, Remediation services and other waste management services = 37168.00

ashe_earn_fai[IOC == "09", avg_salary := 55519.00]
ashe_earn_fai[IOC == "12", avg_salary := 34890.40]
ashe_earn_fai[IOC == "39", avg_salary := 37168.00]

usethis::use_data(ashe_earn_fai, overwrite = TRUE)



