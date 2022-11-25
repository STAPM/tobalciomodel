library(lfsclean)
library(readxl)
library(data.table)
library(magrittr)

### LFS employment data

## construct 4-digit employment by industry-year from the Labour Force Survey

root <- "C:/"
file <- "Users/damon/OneDrive/Documents/Datasets/Labour Force Survey/tab/"
keep_vars <- c("year","quarter","pwt","d_age","d_sex","l_lmstatus","l_full_time","l_sic2007_4dig")

data <- lfsclean::lfsclean(root = root,
                           file = file,
                           year = 2010:2012,
                           ages = 16:89,
                           keep_vars = keep_vars,
                           complete_vars = NULL,
                           deflator = "cpih")


## restrict to all employed/self-employed with complete information on
## full time status and industry

data <- data[l_lmstatus=="employed" | l_lmstatus=="self employed" ,]
data <- data[!is.na(l_full_time) ,]
data <- data[!is.na(l_sic2007_4dig) ,]

## calculate fte employment by industry and quarter

data[l_full_time == "full_time",fte_ := 1]
data[l_full_time == "part_time",fte_ := 0.5]

data[, fte   := sum(pwt*fte_), by = c("time","l_sic2007_4dig")]
data[, total := sum(pwt ),     by = c("time","l_sic2007_4dig")]

data <- unique(data[,c("time","year","l_sic2007_4dig","fte","total")])

## now average employment across quarters within years

data[, fte_   := mean(fte)  , by = c("year","l_sic2007_4dig")]
data[, total_ := mean(total), by = c("year","l_sic2007_4dig")]

data <- unique(data[,c("year","l_sic2007_4dig","fte_","total_")])

setnames(data,
         c("l_sic2007_4dig","fte_","total_"),
         c("SIC_code","fte","total"))

data[, SIC_code := as.numeric(as.character(SIC_code))]

data <- data[order(year,SIC_code),]

########################################################################################
##### Map SIC Employment onto the FAI categories for the alcohol-disaggregated table ###

## read in the SIC - FAI mapping sheet

map <- readxl::read_excel("data-raw/FAI SIC mapping.xlsx",
                          range = "A1:D612",
                          col_names = TRUE)
setDT(map)
map[, SIC_code := as.numeric(SIC_code)]

## read in FAI sectors and get output proportions

sectors <- readxl::read_excel("data-raw/2010_UK_Alcohol_consumption_disaggregated_IxI.xlsx",
                              range = "B5:C111")
setDT(sectors)
setnames(sectors, names(sectors), c("IOC","Sector"))

output <- readxl::read_excel("data-raw/2010_UK_Alcohol_consumption_disaggregated_IxI.xlsx",
                             range = "D120:DE120",
                             col_names = FALSE)
output <- as.vector(as.matrix(output))
output <- cbind(sectors,output)

sec1_alc_prop <- as.numeric(output[61,"output"]/(output[60,"output"] + output[61,"output"]))
sec2_alc_prop <- as.numeric(output[69,"output"]/(output[68,"output"] + output[69,"output"]))
sec3_alc_prop <- as.numeric(output[71,"output"]/(output[70,"output"] + output[71,"output"]))
rm(output)


## loop over years
for (y in 2010:2020) {

  empl <- data[year == y,]

  ## map onto the FAI categories

  merge <- merge.data.table(map, empl, by = "SIC_code", all.x = TRUE)

  merge[is.na(total), total := 0]
  merge[is.na(fte), fte := 0]

  merge[, tot_emp := sum(total, na.rm=TRUE), by = "IOC"]
  merge[, tot_fte := sum(fte, na.rm=TRUE)  , by = "IOC"]

  merge <- unique(merge[,c("IOC","Sector","tot_emp","tot_fte")])

  ### need to create the 3 disaggregated alcohol sectors.

  ### apportion employment in line with the output ratio between the
  ### alcohol and non-alcohol sectors

  fai_empl <- merge.data.table(sectors, merge, by = c("IOC","Sector"), all.x = TRUE, sort = FALSE)

  sec1_alc_emp <- as.numeric(fai_empl[60,"tot_emp"])
  sec2_alc_emp <- as.numeric(fai_empl[68,"tot_emp"])
  sec3_alc_emp <- as.numeric(fai_empl[70,"tot_emp"])

  sec1_alc_fte <- as.numeric(fai_empl[60,"tot_fte"])
  sec2_alc_fte <- as.numeric(fai_empl[68,"tot_fte"])
  sec3_alc_fte <- as.numeric(fai_empl[70,"tot_fte"])

  #### fill in the missing alcohol categories

  ## Wholesale/Retail
  fai_empl[61, tot_emp :=  sec1_alc_emp*sec2_alc_prop]
  fai_empl[60, tot_emp :=  sec1_alc_emp*(1-sec1_alc_prop)]

  fai_empl[61, tot_fte :=  sec1_alc_fte*sec2_alc_prop]
  fai_empl[60, tot_fte :=  sec1_alc_fte*(1-sec1_alc_prop)]

  ## Food and Beverage
  fai_empl[69, tot_emp :=  sec2_alc_emp*sec2_alc_prop]
  fai_empl[68, tot_emp :=  sec2_alc_emp*(1-sec2_alc_prop)]

  fai_empl[69, tot_fte :=  sec2_alc_fte*sec2_alc_prop]
  fai_empl[68, tot_fte :=  sec2_alc_fte*(1-sec2_alc_prop)]

  ## Food and Beverage
  fai_empl[71, tot_emp :=  sec3_alc_emp*sec3_alc_prop]
  fai_empl[70, tot_emp :=  sec3_alc_emp*(1-sec3_alc_prop)]

  fai_empl[71, tot_fte :=  sec3_alc_fte*sec3_alc_prop]
  fai_empl[70, tot_fte :=  sec3_alc_fte*(1-sec3_alc_prop)]


  fai_empl[, year := y]

  assign(paste("lfs_empl_fai",y,sep="."),fai_empl)
}

lfs_empl_fai <- rbindlist(list(lfs_empl_fai.2010,
                               lfs_empl_fai.2011,
                               lfs_empl_fai.2012,
                               lfs_empl_fai.2013,
                               lfs_empl_fai.2014,
                               lfs_empl_fai.2015,
                               lfs_empl_fai.2016,
                               lfs_empl_fai.2017,
                               lfs_empl_fai.2018,
                               lfs_empl_fai.2019,
                               lfs_empl_fai.2020))

#########################################
### 13/08/2022 - update this code to project employment beyond 2020
### based on working futures
###
### https://warwick.ac.uk/fac/soc/ier/researchthemesoverview/wf7downloads/

##usethis::use_data(lfs_empl_fai,overwrite = TRUE)

##################################################################
### Read in Working Futures 7 estimated % change in employment ###

wf <- read_excel("data-raw/UK_MainTables.Main.xlsm",
                 sheet = "Ind T1", range = "S4:S10")

sec <- c("Primary sector and utilities","Manufacturing","Construction",
         "Trade, accomod and transport","Business and other services","Non-marketed services")

perc_changes <- data.table(sec, wf)

setnames(perc_changes, names(perc_changes), c("sec","change"))

perc_changes[, prop_change := 1 + (change/100)]

###################################################
##### Loop over the years and each year adjust employment for the WF forecaste
##### annual proportionate change

data <- copy(lfs_empl_fai)

start_year <- max(data$year) + 1

for (y in start_year:2027){

  t <- y - 1

curr_year <- data[year == t,]

curr_year[c(1:7,52:57), sec := "Primary sector and utilities"]
curr_year[8:51, sec := "Manufacturing"]
curr_year[58, sec := "Construction"]
curr_year[59:71, sec := "Trade, accomod and transport"]
curr_year[c(72:95,100:106), sec := "Business and other services"]
curr_year[96:99, sec := "Non-marketed services"]

update <- merge(curr_year, perc_changes, by = "sec", sort = FALSE, all = TRUE)

update[, tot_emp := tot_emp * prop_change]
update[, tot_fte := tot_fte * prop_change]

update[, year := year + 1]

update[, c("sec","change","prop_change") := NULL]

data <- rbindlist(list(data, update))

}

lfs_empl_fai <- copy(data)

usethis::use_data(lfs_empl_fai,overwrite = TRUE)



