
library(readxl)

### Inflation

inflation <- read.csv("data-raw/inflation.csv")
inflation <- inflation[-c(1:5),]

names(inflation)[names(inflation) == "Title"] <- "year"
names(inflation)[names(inflation) == "CPIH.INDEX.00..ALL.ITEMS.2015.100"] <- "cpih_index"

inflation$year <- as.numeric(as.character(inflation$year))
inflation$cpih_index <- as.numeric(as.character(inflation$cpih_index))

### GDP and GVA

inc <- read_excel("data-raw/gdp and gva estimates.xls",
                  sheet = "A2 AGGREGATES",
                  range = "A6:E78")

inc <- inc[,c("...1","...3","...5")]


names(inc)[names(inc) == "...1"] <- "year"
names(inc)[names(inc) == "...3"] <- "gdp"
names(inc)[names(inc) == "...5"] <- "gva"

### Employment - aggregate the sector level employment in the package data

emp <- rep(NA,10)

emp[1] <- sum(tobalciomodel::employment$fte_2010)
emp[2] <- sum(tobalciomodel::employment$fte_2011)
emp[3] <- sum(tobalciomodel::employment$fte_2012)
emp[4] <- sum(tobalciomodel::employment$fte_2013)
emp[5] <- sum(tobalciomodel::employment$fte_2014)
emp[6] <- sum(tobalciomodel::employment$fte_2015)
emp[7] <- sum(tobalciomodel::employment$fte_2016)
emp[8] <- sum(tobalciomodel::employment$fte_2017)
emp[9] <- sum(tobalciomodel::employment$fte_2018)
emp[10] <- sum(tobalciomodel::employment$fte_2019)

year <- seq(2010,2019,1)

empl <- cbind(year,emp)
empl <- as.data.frame(empl)

#### merge

merge1 <- merge(inflation,inc,by="year")
merge2 <- merge(merge1,empl,by="year")

macro <- merge2

usethis::use_data(macro,overwrite=TRUE)


