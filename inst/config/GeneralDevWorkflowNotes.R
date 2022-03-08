#general dev workflow commands
#=============================
devtools::run_examples()
devtools::document()
devtools::build_vignettes()
devtools::build_readme()
devtools::check()
devtools::build()
devtools::install()

usethis::use_package()
usethis::use_namespace()
usethis::use_version()

1# daily work agreements
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
