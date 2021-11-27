library(lfsclean)
library(readxl)
library(data.table)
library(magrittr)

minyr <- 2010
maxyr <- 2020

############################
### LFS employment data ####

## NOTE: the file:
## data processing - employment data LFS fai.R
##
## must first be run to produce the data tobalciomodel::lfs_empl_fai

empl <- tobalciomodel::lfs_empl_fai

for (y in minyr:maxyr) {

  temp <- empl[year %in% y,]

  temp[1:3   , Industry := "Agriculture"]
  temp[4:57  , Industry := "Production"]
  temp[58    , Industry := "Construction"]
  temp[59:71 , Industry := "Distribution, transport, hotels and restaurants"]
  temp[72:76 , Industry := "Information and communication"]
  temp[77:79 , Industry := "Finance and insurance"]
  temp[80:81 , Industry := "Real estate"]
  temp[82:95 , Industry := "Professional and support activities"]
  temp[96:99 , Industry := "Government, education and health"]
  temp[100:106 , Industry := "Other services"]

  if (y == minyr) {

    empl_data <- copy(temp)

  } else {

    empl_data <- rbindlist(list(empl_data, temp))
  }
}

empl_data <- empl_data[, .(tot_emp = sum(tot_emp),
                           tot_fte = sum(tot_fte)),
                           by = c("year","Industry")]


###############################################
### GVA and output data from the blue book ####

# https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetablessummarytables

minyr <- 1997
maxyr <- 2019

for (y in minyr:maxyr) {

  yr <- as.character(y)

  ##### Output

  out <- readxl::read_excel("data-raw/bb21a10summarytables.xlsx",
                            sheet = yr,
                            range = "C76:L76",
                            col_names = FALSE)
  setDT(out)
  setnames(out,
           names(out),
           c("Agriculture", "Production", "Construction",
             "Distribution, transport, hotels and restaurants",
             "Information and communication", "Finance and insurance",
             "Real estate", "Professional and support activities",
             "Government, education and health", "Other services"))

  out <- melt(out,
              variable.name = "Industry",
              value.name = "output")

  out[, year := y]

  ##### GVA

  gva <- readxl::read_excel("data-raw/bb21a10summarytables.xlsx",
                            sheet = yr,
                            range = "C73:L73",
                            col_names = FALSE)
  setDT(gva)
  setnames(gva,
           names(gva),
           c("Agriculture", "Production", "Construction",
             "Distribution, transport, hotels and restaurants",
             "Information and communication", "Finance and insurance",
             "Real estate", "Professional and support activities",
             "Government, education and health", "Other services"))

  gva <- melt(gva,
              variable.name = "Industry",
              value.name = "gva")

  gva[, year := y]

  #### merge

  data <- merge(out, gva, by = c("year","Industry"), sort = F)

  #### stack data

  if (y == minyr) {

    bb_data <- copy(data)

  } else {

    bb_data <- rbindlist(list(bb_data, data))
  }
}
rm(data, out, gva)

macro_data <- merge(bb_data, empl_data, by = c("year","Industry"), sort = F)

usethis::use_data(macro_data, overwrite = TRUE)
