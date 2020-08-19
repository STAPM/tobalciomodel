#' Price elasticities
#'
#' A data frame containing the price elasticity of demand for five types of alcohol; beer, cider,
#' wine, spirits, and RTDs split by off and on trade sales. Elasticities are obtained from multiple sources.
#'
#' @format A data frame with 10 observations and 4 variables.
#' \describe{
#'     \item{Product}{Beer, Cider, Wine, Spirits, RTDs}
#'     \item{On.Trade}{On-trade alcohol price elasticities}
#'     \item{Off.Trade}{Off-trade alcohol price elasticities }
#'     \item{Source}{Source for the elasticities. Collis et al. (2010) or Meng et al. (2014)}
#' }
"elasticities"


#' Alcohol Consumption Data for England, Wales, and Scotland 2000-2019.
#'
#' A data frame containing alcohol consumption data separately for Scotland, and for England and Wales.
#' Data was compiled by Monitoring and Evaluating Scotlands Alcohol Strategy (MESAS) from Nielsen and CGA Strategy.
#' Off-trade alcohol sales have been adjusted to account for the exclusion of discount retailers.
#' Raw data was obtained from \href{http://www.healthscotland.scot/publications/mesas-monitoring-report-2020}{the MESAS monitoring report 2020.}
#'
#' @format A data frame with 320 observations and 12 variables.
#' \describe{
#'     \item{year}{Demands of Sector A from the row sector.}
#'     \item{product}{Total alcohol, and 7 types of alcohol; spirits, RTDs, fortifies wines, wine, cider, perry, and beer.}
#'     \item{litres.ontrade}{total volume of on-trade alcohol sales (1000 litres)}
#'     \item{litres.offtrade}{total volume of off-trade alcohol sales (1000 litres)}
#'     \item{units.pp.ontrade}{total volume of on-trade alcohol sales (units) per adult (aged 16+ years)}
#'     \item{units.pp.offtrade}{total volume of off-trade alcohol sales (units) per adult (aged 16+ years)}
#'     \item{price.ontrade}{average price per unit of alcohol sold through the on-trade.}
#'     \item{price.offtrade}{average price per unit of alcohol sold through the off-trade.}
#'     \item{population}{total adult population mid-year.}
#'     \item{consumption.ontrade}{total estimated on-trade consumption - units per person X price per unit X population.}
#'     \item{consumption.offtrade}{total estimated off-trade consumption - units per person X price per unit X population.}
#'     \item{country}{indicator. England and Wales, or Scotland}
#' }
"mesas"

#' LFS Employment Data for 106 Sectors.
#'
#' Total employment generated from the quarterly Labour Force Survey (LFS) by 4-digit SIC-2007 industry. Data
#' is then collapsed into the 106 sectors (including 3 alcohol disaggregated sectors) used in the FAI IO table.
#' Employment for the disaggregated sectors is split from the parent sector proportionately by total sectoral
#' output measured in 2010 in the IO table.
#'
#' For each year from 2010 to 2020, total employment and total employment measured in full-time equivalents (FTE)
#' is available (individuals who report being part-time are considered 0.5 FTE)
#'
#' @format A data frame with 106 observations and 23 variables.
"employment"

#' Input-Output Table.
#'
#' A data frame containing relevant data extracted from the publicly available
#' alcohol-disaggregated IO table used by the Fraser of Allender Institute.
#'
#' @format A data frame with 106 observations and 116 variables.
#' \describe{
#'     \item{name}{Name of the SIC07-based sector of economic activity.}
#'     \item{sec1-sec106}{For each sector, a variable capturing inter-industry sales/purchases.}
#'     \item{hhold.demand}{The vector of household demand (a component of final demand).}
#'     \item{final.demand}{The vector of final demands (all non-intermediate demand).}
#'     \item{hhold.output}{The vector of employee compensation - the value added to total output by household labour supply.}
#'     \item{total.output}{The vector of total output - value of raw materials, plus imports, returns to capital and labour, taxes.}
#'     \item{total.demand}{Should equal total output.}
#'     \item{gva.taxes}{gross value added - taxes less subsidies on production component}
#'     \item{gva.wages}{gross value added - employee compensation component}
#'     \item{gva.gos}{gross value added - gross operating surplus component}
#'     \item{gva.total}{gross value added total - sum of employee compensation, gross operating surplus, and taxes on production}
#'     }
"iotable"



#' Scenarios
#'
#' A data frame containing pre-specified input parameters for scenario analyses.
#'
#' @format A data frame with 10 observations and 4 variables.
#' \describe{
#'     \item{year}{year of data to use}
#'     \item{scotland}{TRUE if applying scenario to Scotland}
#'     \item{elasticity}{source of elasticity information}
#'     \item{emp.measure}{measure of employment - fte, employment, or hours}
#'     \item{alc.policy}{alcohol policy scenario - exog, MUP, tax, or NULL for no policy}
#'     \item{tob.policy}{tobacco policy scenario - exog, MUP, tax, or NULL for no policy}
#' }
"scenarios"
