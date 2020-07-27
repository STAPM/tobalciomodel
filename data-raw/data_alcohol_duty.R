library(readxl)


#### Read in the tax year

tax.year <- read_excel(path = paste0("data-raw/","alcohol duty receipts OBR 12-06-2020",".xlsx"),
                   sheet = "alcohol duty receipts OBR 12-06",
                   range = "C1:T1",
                   col_names = FALSE)
tax.year <- as.vector(as.matrix(tax.year))

year <- seq(2000,2017,1)

### Read in the beer duty
beer <- read_excel(path = paste0("data-raw/","alcohol duty receipts OBR 12-06-2020",".xlsx"),
                   sheet = "alcohol duty receipts OBR 12-06",
                   range = "C2:T2",
                   col_names = FALSE)
beer <- as.vector(as.matrix(beer))

### Read in the cider duty
cider <- read_excel(path = paste0("data-raw/","alcohol duty receipts OBR 12-06-2020",".xlsx"),
                   sheet = "alcohol duty receipts OBR 12-06",
                   range = "C4:T4",
                   col_names = FALSE)
cider <- as.vector(as.matrix(cider))

#### Read in the spirits duty

spirits <- read_excel(path = paste0("data-raw/","alcohol duty receipts OBR 12-06-2020",".xlsx"),
                    sheet = "alcohol duty receipts OBR 12-06",
                    range = "C6:T6",
                    col_names = FALSE)
spirits <- as.vector(as.matrix(spirits))

#### Read in the wine duty


wine <- read_excel(path = paste0("data-raw/","alcohol duty receipts OBR 12-06-2020",".xlsx"),
                      sheet = "alcohol duty receipts OBR 12-06",
                      range = "C8:T8",
                      col_names = FALSE)
wine <- as.vector(as.matrix(wine))

#### combine into a data frame

data_alcohol_duty <- data.frame(tax.year,year,beer,cider,spirits,wine)

usethis::use_data(data_alcohol_duty,overwrite = TRUE)
