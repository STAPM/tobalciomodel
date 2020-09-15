
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `iomodeltobalc`

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

**DRAFT WORKING VERSION** - The package is usable but there are still
bugs and further developments that are being worked through i.e. some
code and documentation is still incomplete or in need of being refined.
The code and documentation are still undergoing internal review by the
analyst team.

## Motivation

`tobalciomodel` was created as part of the SPECTRUM work package 4
programme of work on the health economics of tobacco and alcohol at the
School of Health and Related Research (ScHARR), The University of
Sheffield. SPECTRUM WP4 aims to produce analyses of the wider economic
impacts of alcohol and tobacco and prevention policies.

The `tobalciomodel` package has been developed to conduct analysis of
sector-by-sector impacts of alcohol/tobacco prevention policies as well
as impacts on macroeconomic outcomes; employment, GDP, and gross value
added.

## Usage

`tobalciomodel` is a package for producing an input-output analysis of
policies intending to reduce consumption of alcohol and/or tobacco. The
**processes** applied by the functions in `tobalciomodel` give options
to:

1.  Allow for exogenous changes in consumption preferences, or induced
    changes in consumption due to tax changes and MUP type policies.  
2.  Specification of changes in demand for other sectors outputs in
    response to changes in overall expenditure on alcohol and tobacco.  
3.  Introduce changes in government spending which may accompany
    tax/pricing policy changes.

The **output** of these processes is a breakdown of aggregate effects on
GDP, gross value added, and employment of a policy-induced change in
consumption of alcohol and/or tobacco. These effects are decomposed into
direct, indirect, and induced effects

## Installation

`tobalciomodel` can \*\* be installed \*\* from GitLab with:

``` r
#install.packages("devtools")
#install.packages("getPass")

devtools::install_git(
  "https://gitlab.com/SPECTRUM_sheffield/tobalciomodel.git", 
  credentials = git2r::cred_user_pass("uname", getPass::getPass()),
  ref = "x.x.x",
  build_vignettes = TRUE
)

# Where uname is your Gitlab user name.
# ref = "x.x.x" is the version to install - remove this to install the latest version
# this should make a box pop up where you enter your GitLab password
```

Then load the package, and some other packages that are useful.

## Basic Functionality
