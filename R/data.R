#' Average Weekly Earnings (AWE) Index
#'
#' An index of average weekly earnings growth from 2000 to 2020. Used to deflate the package earnings data to any other year used for the
#' input-output analysis. Annual figures taken from the ONS statistical release and accessed 02/11/2021.
#'
#' @source \href{https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/timeseries/ka5h/emp}{AWE: Whole Economy Index}
"awe"

#' Map COICOP to CPA
#'
#' A data table which can be used to map expenditure distributed across 36 COICOP
#' categories onto the 105 CPA categories which are used in the Supply and Use tables.
#'
#' The mapping is derived from Table 3 - Household Final Consumption Expenditure (HhFCE) -
#' of the Supply and Use Tables for 2018. The data can be found \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{here}.
"coicop_cpa_mapping"

#' ASHE Earnings Data 2021
#'
#' Average annual earnings published by the ONS from analysis of the Annual Survey of Hours and Earnings by 4-digit SIC-2007 industry.
#' Data are collapsed into the 105 categories used in the ONS Supply and Use Tables using employment calculated from the Labour Force
#' Survey by industry as weights to calculate average annual earnings by CPA category.
#'
#' Figures are taken for the most recent year for which data is available. As of 05/05/2021 this is the 2020 provisional estimates. The data
#' are obtained \href{https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/industry4digitsic2007ashetable16}{here}.
"ashe_earn_cpa"

#' ASHE Earnings Data 2010-2021
#'
#' Average annual earnings published by the ONS from analysis of the Annual Survey of Hours and Earnings by 4-digit SIC-2007 industry.
#' Data are collapsed into the 106 categories used in the Fraser of Allender Institute (FAI) alcohol disaggregated input-output
#' table. SIC level average earnings are collapsed to FAI sector level using employment calculated from the Labour Force
#' Survey by industry as weights.
#'
#' Figures are taken for the most recent year for which data is available. As of 05/05/2021 this is the 2020 provisional estimates. The data
#' are obtained \href{https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/industry4digitsic2007ashetable16}{here}.
"ashe_earn_fai"

#' Demo Data
#'
#' Synthetic data used to represent a simplified 3-sector economy for demonstration of the methodology used in this package.
#'
"demo_data"

#' LFS Employment Data 2010-2027
#'
#' Total employment is calculated from the quarterly Labour Force Survey (LFS) by 4-digit SIC-2007 industry. Data
#' are collapsed into the 105 CPA categories used in the ONS Supply and Use Tables and for a given year, the average total
#' employment across the four quarters is taken. For 2021 to 2027, employment is estimated based on
#' \href{https://warwick.ac.uk/fac/soc/ier/researchthemesoverview/wf7downloads/}{Working Futures 7} forecasts
#' of the annual percentage change in employment for the UK produced by Warwick Institute for Employment Research
#'
#' Figures are available for total employment and total full-time equivalent (FTE)
#' employment (employees categorised in the LFS as part-time are considered 0.5 FTE).
#'
#' The employment figures are constructed from the LFS micro-data obtained from the UK Data Service
#' and processed using the `lfsclean` package v0.9.0.
#'
"lfs_empl_cpa"

#' LFS Employment Data 2010-2027
#'
#' Total employment is calculated from the quarterly Labour Force Survey (LFS) by 4-digit SIC-2007 industry. Data
#' are collapsed into the 106 industry categories used in the FAI alcohol-disaggregated input-output table. the average total
#' employment across the four quarters is taken. For 2021 to 2027, employment is estimated based on
#' \href{https://warwick.ac.uk/fac/soc/ier/researchthemesoverview/wf7downloads/}{Working Futures 7} forecasts
#' of the annual percentage change in employment for the UK produced by Warwick Institute for Employment Research
#'
#' Figures are available for total employment and total full-time equivalent (FTE)
#' employment (employees categorised in the LFS as part-time are considered 0.5 FTE).
#'
#' The employment figures are constructed from the LFS micro-data obtained from the UK Data Service
#' and processed using the `lfsclean` package v0.9.0.
#'
"lfs_empl_fai"

#' Income Tax Parameters 2010-2020
#'
#' Data table of income tax and national insurance parameters - thresholds and tax rates - used for calculating tax paid on employment incomes
#' based on annual earnings. Data provided for 2010 - 2020
#'
#' @source \href{https://www.gov.uk/government/collections/tax-structure-and-parameters-statistics}{Tax structure and parameters statistics}
"inctax_params"

#' Input-Output Table - UK
#'
#' Input-output flow-table as well as demand and output vectors from the alcohol-disaggregated
#' input-output table constructed by Fraser of Allender Institute (FAI) for the report
#' \href{https://fraserofallander.org/publications/the-economic-impact-of-changes-in-alcohol-consumption-in-the-uk/}{"The economic impact of changes in alcohol consumption in the UK"}. Data originally downloaded from
#' \href{https://pureportal.strath.ac.uk/en/datasets/2010-uk-alcohol-consumption-disaggregated-ixi-table}{here}
#'
"iotable_fai"

#' Input-Output Tables - Northern Ireland
#'
#' Input-output flow-table as well as demand and output vectors derived from data tables provided by the
#' \href{https://www.gov.scot/publications/input-output-latest/}{Northern Ireland Statistics and Research Authority
#' (NISRA)} for the years 2016 and 2017. More information about the
#' tables is available \href{https://www.nisra.gov.uk/statistics/economic-accounts-project/analytical-input-output-tables}{here}.
#'
"iotable_nire"

#' Input-Output Table
#'
#' Input-output flow-table as well as demand and output vectors derived from the ONS supply and use tables
#' 1997 - 2018. In addition to the same variables obtained from the FAI input-output table, additional information
#' is available on purchaser prices, tax as a proportion of the purchaser price, and industry profit margins.
#'
#' Supply and Use Tables are published by the Office for National Statistics. The raw data tables from which this
#' dataset is derived can be found
#' \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{here}.
#'
"iotable_ons"

#' Input-Output Tables - Scotland
#'
#' Input-output flow-table as well as demand and output vectors derived from data tables provided by the
#' \href{https://www.gov.scot/publications/input-output-latest/}{Scottish Government} for the years
#' 1998, 2010 2015, and 2019. More information about the tables is available
#' \href{https://www.gov.scot/publications/about-supply-use-input-output-tables/pages/user-guide-symmetric-tables/}{here}.
#'
"iotable_scot"

#' Macroeconomic Data
#'
#' Employment data from the Labour Force Survey. Total gross value added (GVA) and
#' total output in £m obtained from the supply and use summary tables.
#'
#' All values are totalled by broad industry grouping (10). These data are used to
#' scale calculated economic effects to the size of the economy.
#'
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetablessummarytables}{supply and use summary tables}.
#'
"macro_data"

#' Mapping Products and Sectors
#'
#' Allows mapping between product CPA categories and sector IOC categories by linking both classifications
#' to the standard industrial classification (SIC) 2007.
#'
"sic_cpa_fai_mapping"

#' Tobacco and Alcohol Data 2010-2020
#'
#' Data on tobacco expenditure, consumption and prices obtained from the ONS and HMRC duty receipts and clearance data available in the
#' ONS Tobacco Bulletin and other ONS sources. Data on alcohol expenditures, consumption and prices constructed using
#' price and consumption data published by MESAS - Monitoring and Evaluating Scotland's Alcohol Strategy. MESAS data are combined with
#' data on total duty receipts by HMRC published in the ONS Alcohol Bulletin.
#'
#' The key variables in the data are `exp_bp` and `exp_bp_2020`. These are the estimates of total expenditure in basic prices (market prices
#' net of tax). `exp_bp_2020` applies 2020 prices throughout and is therefore a measure of real expenditures.
#'
#' @source \href{https://www.publichealthscotland.scot/publications/mesas-monitoring-report-2021/}{MESAS monitoring report 2021}.
#' @source \href{https://www.gov.uk/government/statistics/alcohol-bulletin}{Alcohol Bulletin}
#' @source \href{https://www.gov.uk/government/statistics/tobacco-bulletin}{Tobacco Bulletin}.
#' @source \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7cb/mm23}{ONE Consumer Price Index (RPI) time series: All Tobacco}
#' @source \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/czmp}{ONS Average Price - Cigarettes pack of 20}
#'
"tob_alc_data"

#' Household Redistribution Vectors
#'
#' A dataset of vectors which can be used to distribute expenditure of households
#' across 36 COICOP categories of consumer spending. These include pro-rata distributions
#' of household consumption across products according to the 2018 ONS Supply and Use Tables.
#' THese can optionally be specified to include or exclude tobacco and/or alcohol.
#'
"vectors_hhold"

#' Government Redistribution Vectors
#'
#' A dataset of vectors which can be used to distribute government spending across the 105
#' CPA categories. These include pro-rata distributions for central government spending, local
#' government spending, and total public sector spending. Other vectors allow allocation of all
#' spending to public admin, defence, and compulsory social security; education services; health services;
#' caring and social work; cultural services including libraries, museums, archives.
#'
"vectors_govt"
