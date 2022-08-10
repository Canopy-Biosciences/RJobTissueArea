
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RJobTissueArea <img src='man/figures/logo.png' align="right" height="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of RJobTissueArea is to detect tissue on images and calculate
its size.

## Installation

1.  setup PC for using R:

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
    >         install.packages("installr")
    >
    > -   install RTools 35 by executing the following code in RStudio:
    >
    >         installr::install.rtools()

2.  setup R for using this package:

    > -   install internal R tools:
    >
    >         install.packages(c("remotes","BioCmanager"))
    >         remotes::install_version("cli",version = ">= 3.0.0")
    >         remotes::install_version("rlang",version=">= 1.0.0")
    >         remotes::install_version("xfun",">= 0.29", upgrade ="never")
    >         install.packages("devtools")
    >         remotes::install_version("htmltools",">= 0.5.2", upgrade ="never")
    >         remotes::install_cran(c("knitr","tinytex","rmarkdown"))
    >
    > -   install a package needed for accessing the mongoDB
    >
    >         remotes::install_version("mongolite","1.0")
    >
    > -   install packages needed for image processing
    >
    >         remotes::install_version("locfit",version="1.5.9.4")
    >         BiocManager::install("EBImage",force=TRUE)

3.  get and install the actual version of the *RJobTissueArea package*:

    > -   go to Github webpage:
    >
    >     <https://github.com/Canopy-Biosciences/RJobTissueArea/>
    >
    > -   klick on newest release and download it
    >
    > -   choose the tar.gz file and save it on your local disk
    >
    > -   close all RStudio sessions and open a new one
    >
    > -   in RStudio set the path and the name and version of the tar.gz
    >     file on your local disk by editing the following code
    >
    >         package_dir <- choose.dir()
    >         package_name <- "RJobTissueArea"
    >         package_version <- "0.0.0.16."
    >
    > -   install the ZKW distance calculation package:
    >
    >         pkg <- paste0(package_name,"_",package_version,"tar.gz")
    >         pkg_path <- file.path(package_dir, pkg)
    >         remotes::install_local(file.path(pkg_path),dependencies = NA, upgrade = "never")

4.  start your analysis in RStudio:

    > -   in RStudio create a “New Project” and name it approbiately or
    >     select a existing project, where you want to add the distance
    >     calculation
    >
    > -   check global settings if chunk evaluation location is from
    >     project
    >
    > -   create a “New File” and choose the “RMarkdown” file type
    >
    > -   select “From Template” and choose a template
    >     (e.g. “estimateTissueArea”)
    >
    > -   save a copy into your project directory and follow
    >     instructions in the template

💌 *please feedback any issues occurring during installation*

### supporting notes

-   The **developmental package** version can be installed from github
    repository directly. To do so, execute the following code in
    RStudio.:

        devtools::install_github("Canopy-Biosciences/RJobTissueArea")

-   If you need to have the flowCore package installed:

    ``` r
    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

    BiocManager::install("flowCore")
    ```

-   Edit R profil

    > -   open the project R profil
    >
    >         usethis::edit_r_profile("project")
    >
    > -   paste the following commands into the file and save it
    >
    >         usethis::proj_set()
    >         options(stringsAsFactors = FALSE)
    >         options(rmarkdown.html_vignette.check_title = FALSE)

-   If you need to install packages from local tar.gz-files run the
    following in RStudio

``` r
lib.path <- choose.dir()

install.from.path <- function(lib.path = lib.path)
{

  if (length(lib.path) != 1) {
    stop("Please specify exactly one source directory for the R packages!")
  }

  if (!file.exists(lib.path)){
    stop("This directory doesn't exist!")
  }

  pkg_installed <- installed.packages()%>%
    as.data.frame()

  packages <- list.files(path = lib.path, pattern = ".*\\.tar.gz$", full.names = TRUE)

  pkg_names <- packages%>%
    basename()%>%
    stringr::str_split("_",simplify=TRUE)

  if (length(packages) == 0){
    stop("No R packages found in the specified directory!")
  }

   for (p in 1:length(packages)){
     if (pkg_names[p,1] %in% pkg_installed$Package == FALSE){

       remotes::install_local(packages[p],
                              dependencies= NA,
                              upgrade = "never")
     }
   }
}

install.from.path(lib.path)
```

## HowToUse

**set input**

-   set chip_group_ID

``` r
group_ID <- "P1761451"
```

-   set and check output directory

``` r
output_dir <- "data_output"
RJobTissueArea:::create_working_directory(output_dir)
```

**get all valid chip_IDs in chip_group**

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
process_TissueDetection(image_groups[1,],
                        output_dir,
                        sigma = 15,
                        threshold = 4,
                        window = 50,
                        plot_image = TRUE,
                        result_ID = group_ID)
```

This example code is also content of the Rmd template
“estimateTissueArea”, provided within the package installation.
