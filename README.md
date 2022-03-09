
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RJobTissueArea <img src='man/figures/logo.png' align="right" height="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of RJobTissueArea is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("juleZWK/RJobTissueArea")
```

## HowToUse

``` r
library(RJobTissueArea)
```

-   get all valid chip_IDs defined in chip_group

``` r
group_ID <- "P1761451"
chip_IDs <- find_valid_group_chip_IDs(group_ID)
```
