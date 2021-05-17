#' Map COICOP to CPA
#'
#' A data table which can be used to map expenditure distributed across 36 COICOP
#' categories onto the 105 CPA categories which are used in the Supply and Use tables.
#'
#' The mapping is derived from Table 3 - Household Final Consumption Expenditure (HhFCE) -
#' of the Supply and Use Tables for 2018.
"coicop_cpa_mapping"

#' ASHE Earnings Data
#'
#' Average annual earnings published by the ONS from analysis of the Annual Survey of Hours and Earnings by 4-digit SIC-2007 industry.
#' Data are collapsed into the 105 categories used in the ONS Supply and Use Tables using employment calculated from the Labour Force
#' Survey by industry as weights to calculate average annual earnings by CPA category.
#'
#' Figures are taken for the most recent year for which data is available. As of 05/05/2021 this is the 2020 provisional estimates. The data
#' are obtained \href{https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/industry4digitsic2007ashetable16}{here}.
"ashe_earn_cpa"

#' LFS Employment Data
#'
#' Total employment is calculated from the quarterly Labour Force Survey (LFS) by 4-digit SIC-2007 industry. Data
#' are collapsed into the 105 CPA categories used in the ONS Supply and Use Tables and for a given year, the average total
#' employment across the four quarters is taken.
#'
#' Figures are available for total employment and total full-time equivalent (FTE)
#' employment (employees categorised in the LFS as part-time are considered 0.5 FTE).
#'
#' The employment figures are constructed from the LFS micro-data obtained from the UK Data Service
#' and processed using the `lfsclean` package v0.9.0.
#'
"lfs_empl_cpa"

#' LFS Employment Data
#'
#' Total employment is calculated from the quarterly Labour Force Survey (LFS) by 4-digit SIC-2007 industry. Data
#' are collapsed into the 106 industry categories used in the FAI alcohol-disaggregated input-output table. the average total
#' employment across the four quarters is taken.
#'
#' Figures are available for total employment and total full-time equivalent (FTE)
#' employment (employees categorised in the LFS as part-time are considered 0.5 FTE).
#'
#' The employment figures are constructed from the LFS micro-data obtained from the UK Data Service
#' and processed using the `lfsclean` package v0.9.0.
#'
"lfs_empl_fai"

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
