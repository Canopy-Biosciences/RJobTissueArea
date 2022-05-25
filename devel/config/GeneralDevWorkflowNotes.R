#RProfile commands
#=================
source("renv/activate.R")
usethis::proj_set()
utils::file.edit("NEWS.md")

# install packages
#=================
#renv::install("")
#renv::install("bioc::imager")
#usethis::use_package()
#devtools::install_bioc()
#devtools::install_github()
#devtools::install()

#general dev workflow commands
#=============================
devtools::run_examples()
renv::snapshot()
devtools::document()

usethis::use_namespace()
devtools::build_vignettes()
devtools::build_readme()
devtools::check()
devtools::build()
devtools::install(dependencies =TRUE, build_vignettes = TRUE)

usethis::use_package()

usethis::use_version()
renv::restore()
#_____________________________
#1) add package to Description----

library(magrittr)
pkg_name <- "RJobTissueArea"
exact_version <- c("mongolite","locfit","EBImage")

deps<-renv::dependencies()
inst_pkg <- installed.packages()%>%as.data.frame()
deps <- dplyr::left_join(deps,
                         inst_pkg%>%
                           dplyr::select(Package,"InstalledVersion"="Version"))


deps_import <- deps%>%
  dplyr::filter(!stringr::str_detect(Source,"devel"))%>%
  dplyr::group_by(Package)%>%
  dplyr::summarize(Version = unique(InstalledVersion))%>%
  dplyr::filter(Package != pkg_name)

purrr::walk(deps_import$Package,
            ~usethis::use_package(.x,"imports"))

purrr::walk2(exact_version,
             rep(TRUE,length(exact_version)),
            ~usethis::use_package(.x,"imports",.y))

#__________________
#2) download tar.gz----
CRAN <- "https://cran.rstudio.com/src/contrib"
lib_path <- "devel/pkg_sources"
RJobTissueArea:::create_working_directory(lib_path)
p <- available.packages()

deps_import <- deps_import%>%
  dplyr::mutate(pkg_folder = paste0(Package,"_",Version,".tar.gz"))%>%
  dplyr::mutate(pkg_URL = paste0(CRAN,"/",pkg_folder))

# then download
e <- purrr::walk(deps_import$pkg_URL,
                 ~try(
                   download.file(
                     url = .x,
                     destfile = paste0(file.path(lib_path,
                                                 basename(.x))))))

# check what is present
down_files <- list.files(lib_path)

# select packages to download from archieve
to_load <- deps_import%>%
  dplyr::filter(!pkg_folder %in% down_files)%>%
  dplyr::mutate(pkg_URL=paste0(CRAN,"/Archive/",Package,"/",pkg_folder))

# then download
e <- purrr::walk(to_load$pkg_URL,
                 ~try(
                   download.file(
                     url = .x,
                     destfile = paste0(file.path(lib_path,
                                                 basename(.x))))))

# check success
down_files <- list.files(lib_path)

# list of tar.gz files which needs manual download
to_load <- deps_import%>%
  dplyr::filter(!pkg_folder %in% down_files)

# daily work agreements
#======================
# increment version at first each day and push
#
# at the end of the day:
#   + check the package and fix errors, warnings, notes instantly
#   + build the package
#   + comit and push
#
# any time an error occure, consider writing a test example at best before fixing
#
# tansfer each task in a helper or internal function or updating existing functions, documentation instantly
#
# use the template available:
#    + Amain_script_function
#    + Bhelper_script_function
#    + Cinternal_script_function
#
# add new variable names to globals.R
#
# anytime a external packahge us used,
#    + include it into Description using: usethis::use_package AND
#    + add manually to the list in initialize_package_project.R
#    # and update package_dependencies
#
# allways address external package functions with pkgname::function
# internaly refer to own, not exported package function with :::

# package function hierarchy:
# roxygen @export: in help file und verfügbar für alle
# roxygen @export und @keywords internal: verfügbar für alle, anzeige in snippets, keine auflistung in helpfile (man.Rd wird aber generiert)
# roxygen @export und @keywords internal und @family ..., verlinkung des man.Rd files bei jenen funktionen in dem helpfile, die zur selben familie gehören und nicht internal
# ohne @export: keine man.Rd erstellung, referenzierung über package:::function, auch in den examples

# data
# usethis::use_data(internal=TRUE) -> werden alle in einem sysdata.rda file im R folder abgespeichert und können einzelln mit package:::dataname aufgerufen werden
# usethis::use_data() -> werden einzelln in data als .rds file abgespeichert, können über data("dataname") aufgerufen werden, benötigen jeweils einen eigenen roxygen abschnitt, im data.R file
