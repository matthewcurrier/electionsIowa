
<!-- README.md is generated from README.Rmd. Please edit that file -->

# electionsIowa

<!-- badges: start -->

<!-- badges: end -->

The goal of electionsIowa is to provide access to cleaned up Iowa
election datasets.

## Installation

You can install the development version of electionsIowa like so:

``` r
devtools::install_github("matthewcurrier/electionsIowa")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(arrow)
#> 
#> Attaching package: 'arrow'
#> The following object is masked from 'package:utils':
#> 
#>     timestamp
library(here)
#> here() starts at I:/2026/p/projects/electionsIowa

df <- read_parquet(here("inst", "data", "parquet", "2020-general.parquet"))
```
