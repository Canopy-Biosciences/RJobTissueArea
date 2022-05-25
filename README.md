
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RJobTissueArea <img src='man/figures/logo.png' align="right" height="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of RJobTissueArea is to …

## Installation

1.  setup your system:

    > -   install R 3.6.3
    >
    >     <https://cran.r-project.org/bin/windows/base/old/3.6.3/>
    >
    > -   install RStudio
    >
    >     <https://www.rstudio.com/products/rstudio/download/#download>
    >
    > -   install the R packages needed for installation by executing
    >     the following code in RStudio:
    >
    >         install.packages(c("devtools","installr","remotes"))
    >
    > -   install RTools 35 by executing the following code in RStudio:
    >
    >         installr::install_rtools()
    >
    > -   install the R package mongolite 1.0 by executing the following
    >     code in RStudio:
    >
    >          remotes::install_version("mongolite","1.0")
    >
    > -   install Bioconductor packages
    >
    >          remotes::install_version("locfit",version="1.5.9.4")
    >
    >          BiocManager::install("EBImage",force=TRUE)

2.  get and install the actual version of the *ZKW TissueArea
    calculation R package*:

    > -   go to Github webpage:
    >
    >     <https://github.com/juleZWK/RJobTissueArea/>
    >
    > -   klick on newest release and download it
    >
    > -   choose the tar.gz file and save it on your local disk
    >
    > -   in RStudio set the path and the name of the tar.gz file
    >
    >         package_dir <- choose.dir()
    >         package_name <- "RJobTissueArea_0.0.0.13.tar.gz"
    >
    > -   install the ZKW distance calculation package by executing the
    >     following code in RStudio:
    >
    >         devtools::install(file.path(package_dir, package_name), dependencies = TRUE)

3.  start your analysis in RStudio:

    > -   in RStudio create a “New Project” and name it approbiately or
    >     select a existing project, where you want to add the distance
    >     calculation
    >
    > -   create a “New File” and choose the “RMarkdown” file type
    >
    > -   select “From Template” and choose the “ExampleReport” template
    >
    > -   save a copy into your project directory and follow
    >     instructions in the template

💌 *please feedback any issues occurring during installation*

### supporting notes

-   The **developmental package** version can be installed from github
    repository directly. To do so, execute the following code in
    RStudio.:

        devtools::install_github("juleZWK/RJobTissueArea")

-   If you need to have the flowCore package installed:

    ``` r
    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

    BiocManager::install("flowCore")
    ```

## HowToUse

**set input**

-   set chip\_group\_ID

``` r
group_ID <- "P1761451"
```

-   set and check output directory

``` r
output_dir <- "data/data_output"
RJobTissueArea:::create_working_directory(output_dir)
```

**get all valid chip\_IDs in chip\_group**

``` r
chip_IDs <- find_valid_group_chip_IDs(group_ID)
```

**create ScanHistory**

``` r
ScanHistory <- create_ScanHistory_extended(chip_IDs,
                                           output_dir,
                                           result_ID = group_ID)
```

**select valid hdr image groups**

``` r
image_groups <- create_hdr_image_groups(ScanHistory)
```

**apply tissue detection workflow**

``` r
process_TissueDetection(image_groups,
                        output_dir,
                        sigma = 15,
                        threshold = 4,
                        window = 50,
                        plot_image = TRUE,
                        result_ID = group_ID)
```

# supplement

-   read ScanHistory file from output\_dir

``` r
ScanHistory <- read_ScanHistory(group_ID,
                                output_dir)
```
