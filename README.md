
<!-- README.md is generated from README.Rmd. Please edit that file -->

# electionsIowa

<!-- badges: start -->

<!-- badges: end -->

The goal of electionsIowa is to provide access to cleaned up Iowa
election datasets. You will need to download election result files
yourself. Iowa elections results can be found
**[here](https://sos.iowa.gov/iowans/election-results-statistics)**.

## Installation

You can install the development version of electionsIowa like so:

``` r
devtools::install_github("matthewcurrier/electionsIowa")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(arrow)
library(here)

df <- read_parquet(here("inst", "data", "parquet", "2020-general.parquet"))
```
