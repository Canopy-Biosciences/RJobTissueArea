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
devtools::install(dependencies =FALSE, build_vignettes = FALSE)

options(rmarkdown.html_vignette.check_title = FALSE)
getwd()
usethis::use_package()

usethis::use_version()
renv::restore()

#_________
#sinew----
pkg_dir <- usethis::proj_get()
pkg_dir_R <- file.path(pkg_dir,'R')
pkg_dir_r <- file.path(pkg_dir,'R')
#Package Maintenance
#
#Update functions as needed
#Run on updated functions:
Rfiles <- list.files("R")

sinew::moga(file.path(pkg_dir_r,"helpers_NameHarmonisation.R"),overwrite = FALSE)
purrr::walk(list.files("R"),
            ~sinew::moga(file.path(pkg_dir_r,.x),
                 overwrite = TRUE))
sinew::interOxyAddIn()
sinew::update_desc(path = pkg_dir_r, overwrite = TRUE)
#Fill in the relevant descriptions and examples.
#If needed create the correct Imports in the DESCRIPTION file.:
sinew::make_import(pkg_dir_R,format = 'description',desc_loc = pkg_dir)
sinew::make_import(script = file.path(pkg_dir,
                                      ".R") ,format = 'oxygen')
#get all params
sinew::make_import(script = pkg_dir_r,format = 'oxygen')

#Build the Package


#__________________
#dependencies----
exact_version = c("mongolite","locfit","EBImage")
ignore_folder <- c("devel")
ignore_package <- "RJobTissueArea"
#0) get and check dependencies----
dep <-get_package_dependencies(exact_version,ignore_folder,ignore_package)
#1) add pkgs to description----
add_dependencies_to_DESCRIPTION(exact_version,ignore_folder,ignore_package)
#2) download tar.gz----
download_pkg_sources(dep$Package,dep$Version)

#
#pkg dok data----
tempfile()
dput
dget
deparse
dump
dump(ls(as.environment("package:RJobTissueArea")))
readLines()

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
# avoid adding dd new variable names to globals.R, INSTEAD
# visible binding of variables in functions definition ELSE
# adding variables as data to package AND refer using rlang::.data AND
# adding GlobalVariables names() add package dok
#
# anytime a external package us used,
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

# @seealso() [funktionname()] - verlinkung zur doc von dieser fkt
# data
# usethis::use_data(internal=TRUE) -> werden alle in einem sysdata.rda file im R folder abgespeichert und können einzelln mit package:::dataname aufgerufen werden
# usethis::use_data() -> werden einzelln in data als .rds file abgespeichert, können über data("dataname") aufgerufen werden, benötigen jeweils einen eigenen roxygen abschnitt, im data.R file
