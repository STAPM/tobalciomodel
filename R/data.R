#' Alcohol duties as of September 2020.
#'
"data_alcohol_duty"

#' Alcohol Consumption Data for England and Wales 2000-2019.
#'
"data_mesas_englandwales"

#' Fraser of Allender Institute (FAI) Input-Output Table.
#'
"data_iotable_fai"

#' Office for National Statistics (ONS) Input-Output Tables.
#'
"data_iotables_ons"


#' Alcohol own-price elasticities.
#'
#' Price elasticity of demand for 10 alcohol products, taken from Meng et al. (2014)
#'
"elasticities"

#' LFS Employment Data for 106 Sectors.
#'
#' Total employment generated from the quarterly Labour Force Survey (LFS) by 4-digit SIC-2007 industry. Data
#' is then collapsed into the 106 sectors (including 3 alcohol disaggregated sectors) used in the FAI IO table.
#' Employment for the disaggregated sectors is split from the parent sector proportionately by total sectoral
#' output measured in 2010 in the IO table.
#'
#' For each year from 2010 to 2019, total employment and total employment measured in full-time equivalents (FTE)
#' is available (individuals who report being part-time are considered 0.5 FTE)
#'
#' @format A data frame with 106 observations and 23 variables.
"employment"

#' Macroeconomic Data.
#'
#' Total employment generated from the quarterly Labour Force Survey (LFS) by 4-digit SIC-2007 industry. Data
#' is then collapsed into the 106 sectors (including 3 alcohol disaggregated sectors) used in the FAI IO table.
#' Employment for the disaggregated sectors is split from the parent sector proportionately by total sectoral
#' output measured in 2010 in the IO table.
#'
#' For each year from 2010 to 2019, total employment and total employment measured in full-time equivalents (FTE)
#' is available (individuals who report being part-time are considered 0.5 FTE)
#'
#' @format A data frame with 10 observations and 5 variables.
#' \describe{
#'     \item{year}{}
#'     \item{cpih_index}{inflation index (base year 2010)}
#'     \item{gdp}{nominal Gross Domestic Product (£millions)}
#'     \item{gva}{nominal Gross Value Added (£millions)}
#'     \item{emp}{aggregate employment (full-time equivalents)}
#'     \item{gdp_real}{Real Gross Domestic Product in 2010 prices (£millions)}
#'     \item{gva_real}{Real Gross Value Added in 2010 prices (£millions)}
#' }
"macro"

