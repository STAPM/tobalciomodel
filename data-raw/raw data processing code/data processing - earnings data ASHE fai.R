library(readxl)
library(data.table)
library(lfsclean)
library(magrittr)

###################################################
## read in the SIC - FAI sectors mapping sheet ####

map <- read.csv("data-raw/FAI SIC mapping.csv")
setDT(map)
setnames(map, names(map), c("Industry","SIC_code","IOC","Sector"))
map[, SIC_code := as.numeric(SIC_code)]

###################################
### read in LFS employment data ###

### need employment to use as weights when mapping earnings to CPA but in ASHE data
### much of the njobs entries are missing, so match in FTE employment from the BRES/LFS data
### for the most recent year of data. Use LFS FTE totals as weights

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

## finally average across years

lfs_empl <- data[, .(fte = mean(fte, na.rm = TRUE),
                     total = mean(total,na.rm = TRUE)),
                 by = "SIC_code"]
rm(data, file, root, vars)

###############################################
### read in the ASHE earnings data ############

filepath <- "D:/Datasets/Annual Survey of Hours and Earnings/by industry/"

y <- 2010


### read in and clean
for (y in 2010:2020) {
earn <- readxl::read_excel(paste0(filepath,y,"/SIC2007 Table 16.7a   Annual pay - Gross ",y,".xls"),
                           sheet = "All",
                           range = "A5:S997",
                           col_names = TRUE)
setDT(earn)


earn <- earn[,c(1:3,6)]

setnames(earn,
         names(earn),
         c("Industry","SIC_code","njobs","salary"))

earn1 <- earn[SIC_code %in% c("A","B","C","D","E","F","G","H","I","J",
                              "K","L","M","N","O","P","Q","R","S","T"),]

earn[, SIC_code := as.numeric(SIC_code)]
earn[, salary := as.numeric(salary)]
earn[, njobs := as.numeric(njobs)]

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

earn1 <- merge(earn1, sic1, by = c("Industry","SIC_code"), all.x = F, all.y = T, sort = F)
earn1[, c("Industry","Digit4","Digit3","Digit2","Digit1","njobs") := NULL]
setnames(earn1, c("SIC_code","salary"), c("SIC_code_1dig","salary_1dig"))

earn = merge(earn4, earn3, by = "SIC_code_3dig", all.x = TRUE)
earn = merge(earn , earn2, by = "SIC_code_2dig", all.x = TRUE)
earn = merge(earn , earn1, by = "SIC_code_1dig", all.x = TRUE)

## if 4-digit SIC earnings are missing, impute using the next level up.

earn[is.na(salary), salary := salary_3dig ]
earn[is.na(salary), salary := salary_2dig ]
earn[is.na(salary), salary := salary_1dig ]

### merge earnings data to the FAI mapping

earn <- merge(earn, map, by = c("SIC_code"))

### merge employment and earnings data

data <- merge(earn, lfs_empl, by = c("SIC_code"), all.x = TRUE)

### collapse to FAI industry level. merge to the FAI sectors (set weights to 1 for Agri since these are NA)

data[IOC == "01", fte := 1]
data <- data[, .(avg_salary = weighted.mean(salary, w = fte, na.rm=TRUE) ) ,
             by = c("IOC","Sector")]

sectors <- readxl::read_excel("data-raw/2010_UK_Alcohol_consumption_disaggregated_IxI.xlsx",
                              range = "B5:C111")
setDT(sectors)
setnames(sectors, names(sectors), c("IOC","Sector"))

ashe_earn_fai <- merge(sectors, data[,c("IOC","avg_salary")], by = "IOC", all.x = T, sort = F)

# fill in alcohol disaggregated industries with the same wage as the aggregated sector.

sec1 <- as.numeric(ashe_earn_fai[IOC == 46, "avg_salary"])
sec2 <- as.numeric(ashe_earn_fai[IOC == 53, "avg_salary"])
sec3 <- as.numeric(ashe_earn_fai[IOC == 55, "avg_salary"])


ashe_earn_fai[61, avg_salary := sec1]
ashe_earn_fai[69, avg_salary := sec2]
ashe_earn_fai[71, avg_salary := sec3]

### save out the dataset
ashe_earn_fai[, year := y]

assign(paste0("a",y),ashe_earn_fai)
rm(ashe_earn_fai)
}

ashe_earn_fai <- rbindlist(list(a2010,a2011,a2012,a2013,a2014,a2015,
                                a2016,a2017,a2018,a2019,a2020))

usethis::use_data(ashe_earn_fai, overwrite = TRUE)



