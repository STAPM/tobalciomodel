library(curl)
library(magrittr)
library(readxl)
library(data.table)
library(ggplot2)
library(htmltools)

data <- readRDS("data-raw/lfs_data_nireland.rds")

##########################################
## read in the SIC mapping lookup sheet

map <- readxl::read_excel("data-raw/SIC_to_IOTABLE_mapping.xlsx",
                          range = "A2:H614",
                          col_names = TRUE) %>% setDT

## retain Scottish lookup only
map <- map[, c("IOC","Sector","IOC_scot","Sector_scot") := NULL]
setnames(map, c("IOC_ni","Sector_ni"), c("IOC","Sector"))

map[, SIC_code := as.numeric(SIC_code)]

###########################################
## loop over years - match LFS employment to the lookup table, collapse to
## Scottish IO table layout

for (y in 2010:2020) {

  empl <- data[year == y,]

  ##############################################
  ## Map employment onto the IO table sectors

  merge <- merge.data.table(map, empl, by = "SIC_code", all = TRUE)

  merge[is.na(total_), total_ := 0]
  merge[is.na(fte_), fte_ := 0]

  merge <- merge[, .(tot_emp = sum(total_, na.rm = TRUE),
                     tot_fte = sum(fte_,   na.rm = TRUE) ), by = c("IOC","Sector")]

  ### This will have 63 sectors.

  ni_empl <- copy(merge)

  ni_empl[, year := y]

  if (y == 2010){

    lfs_empl_ni <- copy(ni_empl)
  } else {

    lfs_empl_ni <- rbindlist(list(lfs_empl_ni, ni_empl))
  }

}

lfs_empl_ni[, country := "N_Ireland"]
