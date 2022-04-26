
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

-   set and check output directory

``` r
output_dir <- "inst/data_output"
check_working_directory(output_dir)
```

-   create and export list of all chip group image files

``` r
image_files <- export_list_all_image_files(chip_IDs)
```

-   load list of all image files

``` r
file <- create_result_filepath(output_dir,
                               name_string = "all_image_files_of_groupID",
                               result_ID=group_ID,
                               type = ".csv")
image_files <- data.table::fread(file)
```

-   select valid bolb32 files

``` r
blob32_files <- select_valid_image_files(image_files,
                                         type = "blob32")
```
