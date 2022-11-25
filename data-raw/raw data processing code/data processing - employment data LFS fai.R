library(curl)
library(magrittr)
library(readxl)
library(data.table)
library(ggplot2)
library(htmltools)

data <- readRDS("data-raw/lfs_data_uk.rds")

###################################
## read in the SIC mapping sheet

map <- readxl::read_excel("data-raw/SIC_to_IOTABLE_mapping.xlsx",
                          range = "A2:D614",
                          col_names = TRUE) %>% setDT

map[, SIC_code := as.numeric(SIC_code)]

###################################################
## read in FAI sectors and get output proportions
## between alcohol and non-alcohol sub-sectors

url <- "https://pureportal.strath.ac.uk/files/86400329/2010_UK_Alcohol_consumption_disaggregated_IxI.xlsx"
temp <- tempfile()
temp <- curl_download(url = url, destfile = temp, quiet = FALSE, mode = "wb")

sectors <- readxl::read_excel(temp,
                              range = "B5:C111") %>% setDT

setnames(sectors, names(sectors), c("IOC","Sector"))

output <- readxl::read_excel(temp,
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

  ##############################################
  ## Map employment onto the IO table sectors

  merge <- merge.data.table(map, empl, by = "SIC_code", all.x = TRUE)

  merge[is.na(total), total := 0]
  merge[is.na(fte), fte := 0]

  merge <- merge[, .(tot_emp = sum(total, na.rm = TRUE),
                     tot_fte = sum(fte,   na.rm = TRUE) ), by = c("IOC","Sector")]

  ### This will have 103 sectors. Need to create the 3 disaggregated alcohol sectors.

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

  if (y == 2010){

  lfs_empl_fai <- copy(fai_empl)
  } else {

  lfs_empl_fai <- rbindlist(list(lfs_empl_fai, fai_empl))
  }

}

lfs_empl_fai[, country := "UK"]
