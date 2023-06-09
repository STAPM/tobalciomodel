% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tobalciomodel.R
\name{IOModel}
\alias{IOModel}
\title{Input-Output Wrapper Function}
\usage{
IOModel(
  FAI = FALSE,
  year_sut = 2018,
  year = 2019,
  fte = TRUE,
  hhold_exp = c(-100, -100, -100),
  govt_revenue = 0,
  hhold_passthru = 1,
  govt_passthru = 0,
  hhold_reallocate = 3,
  govt_reallocate = 1,
  tax_data = tobalciomodel::inctax_params
)
}
\arguments{
\item{FAI}{Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table. If FALSE, uses the
ONS supply and use tables. Defaults to FALSE}

\item{year_sut}{Integer. Select the year (from 1997 to 2018) ONS supply and use tables to use in the analysis.}

\item{year}{Integer. Select the year (from 2016 to 2020) of alcohol, tobacco, earnings and employment data to use in the analysis.}

\item{fte}{Logical. If TRUE (default) use full-time equivalent (FTE) employment, if FALSE
use total employees.}

\item{hhold_exp}{Numeric vector of length 3. Change in household consumption measured in basic prices for - in order - off-trade alcohol,
on-trade alcohol, and tobacco.}

\item{govt_revenue}{Numeric. change in government revenues, measured in basic prices.}

\item{hhold_passthru}{Numeric. Assumed household rate of passthrough - the proportion of change in spending which
is compensated for in spending on other consumption categories. Defaults to 1 (full passthrough).}

\item{govt_passthru}{Numeric. Assumed government rate of passthrough - the proportion of change in revenues
which are matched by changes in government expenditure. Defaults to 0 (no passthrough).}

\item{hhold_reallocate}{Numeric (1-3). The distribution of reallocation of spending to implement from the \code{vectors_hhold} data.
Option 1 allocates pro-rata across all consumption categories, option 2 excludes alcohol and tobacco consumption, option 3
(default) further excludes health, education, rents and utilities.}

\item{govt_reallocate}{Numeric (1-5). The distribution of reallocation of spending to implement from the \code{vectors_govt} data.
Option 1 (default) allocates pro-rata according to the distribution of total government spending,
option 2 allocates according to central government spending only, option 3 allocates according
to local government only. Option 4 allocates all spending to health, and option 5 allocates all spending
to education.}

\item{tax_data}{Data table. Package data containing the necessary parameters to calculate
income tax and national insurance contributions from annual earnings data}
}
\value{
Excel table of results
}
\description{
Read in inputs, make calculations, and return clean model outputs for an input-output analysis. Wrapper
function for the other functions in the package to implement the full workflow. This function takes parameter
inputs determined externally (e.g. from processed outputs obtained via the Sheffield Tobacco and Alcohol Policy Model
- STAPM).
}
\examples{

\dontrun{


}
}
\author{
Damon Morris
}
