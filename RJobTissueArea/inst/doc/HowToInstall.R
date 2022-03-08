## ---- include=FALSE-----------------------------------------------------------
usethis::proj_set()
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache=FALSE
)

knitr::opts_knit$set(cache=FALSE,
                     root.dir=file.path(getwd()),
                     base.dir=file.path(getwd())
)

here::here()
here::i_am("vignettes/HowToInstall.Rmd")


## ----eval=FALSE---------------------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  
#  BiocManager::install("flowCore")

