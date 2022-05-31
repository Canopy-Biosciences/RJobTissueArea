## ----setup, include = FALSE---------------------------------------------------
# With the root.dir option below,
# this vignette runs the R code in a temporary directory
# so new files are written to temporary storage
# and not the user's file space.
package_dir <- rprojroot::find_rstudio_root_file()
dir <- file.path(package_dir)#fs::dir_create(tempfile())
knitr::opts_knit$set(root.dir = dir,
                     base.dir=getwd())
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  root.dir=dir,
  base.dir=getwd())

Sys.setenv(TAR_WARN = "false")

here::i_am("vignettes/AnalysisWorkflow.Rmd")

if (identical(Sys.getenv("NOT_CRAN", unset = "false"), "false")) {
  knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
#  library(targets)
#  library(RJobTissueArea)

## -----------------------------------------------------------------------------
#  tar_unscript()

## ---- echo = FALSE,message=FALSE----------------------------------------------
#  library(targets)
#  library(magrittr)
#  library(dplyr)
#  library(ggplot2)
#  tar_script({
#  
#    options(crayon.enabled = FALSE)
#    tar_option_set(memory = "transient", garbage_collection = TRUE)
#    tar_option_set(packages = c("RJobTissueArea","tibble", "readr", "dplyr", "ggplot2"),
#                   imports = c("RJobTissueArea"))
#  
#    #load all package functions
#    Rfiles <- list.files("R")
#    purrr::walk(Rfiles,~source(file.path("R",.x)))
#  
#    create_plot <- function(data) {
#      ggplot(data) +
#        geom_histogram(aes(x = Ozone), bins = 12) +
#        theme_gray(24)
#    }
#    list(
#      tar_target(raw_data, airquality),
#      tar_target(data,
#                 raw_data %>%
#                   filter(!is.na(Ozone))%>%
#                   dplyr::slice(1:20)),
#      tar_target(hist,
#                 data%>%
#                   create_plot()),
#      tar_target(fit, biglm::biglm(Ozone ~ Wind + Temp, data))
#    )
#  
#  })

## -----------------------------------------------------------------------------
#  tar_target(fit2, biglm::biglm(Ozone ~ Wind + Temp, data))

## ----output =FALSE------------------------------------------------------------
#  tar_make()

## ---- message = FALSE---------------------------------------------------------
#  library(biglm)
#  raw_data<-tar_read(raw_data)
#  tar_read(fit)

## -----------------------------------------------------------------------------
#  tar_read(hist)

## -----------------------------------------------------------------------------
#  tar_visnetwork()

