library(curl)
library(magrittr)
library(readxl)
library(data.table)
library(ggplot2)
library(htmltools)

data <- readRDS("data-raw/lfs_data_scotland.rds")

##########################################
## read in the SIC mapping lookup sheet

map <- readxl::read_excel("data-raw/SIC_to_IOTABLE_mapping.xlsx",
                          range = "A2:H614",
                          col_names = TRUE) %>% setDT

## retain Scottish lookup only
map <- map[, c("IOC","Sector","IOC_ni","Sector_ni") := NULL]
setnames(map, c("IOC_scot","Sector_scot"), c("IOC","Sector"))

map[, SIC_code := as.numeric(SIC_code)]

###########################################
## loop over years - match LFS employment to the lookup table, collapse to
## Scottish IO table layout

for (y in 2010:2020) {

  empl <- data[year == y,]

  ##############################################
  ## Map employment onto the IO table sectors

  merge <- merge.data.table(map, empl, by = "SIC_code", all.x = TRUE)

  merge[is.na(total_), total_ := 0]
  merge[is.na(fte_), fte_ := 0]

  merge <- merge[, .(tot_emp = sum(total_, na.rm = TRUE),
                     tot_fte = sum(fte_,   na.rm = TRUE) ), by = c("IOC","Sector")]

  ### This will have 99 sectors.

  scot_empl <- copy(merge)

  scot_empl[, year := y]

  if (y == 2010){

    lfs_empl_scot <- copy(scot_empl)
  } else {

    lfs_empl_scot <- rbindlist(list(lfs_empl_scot, scot_empl))
  }

}

lfs_empl_scot[, country := "Scotland"]
