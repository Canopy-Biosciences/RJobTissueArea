#_______
#HOWTOUSE----

#script au?erhalb eines projectes ?ffnen
#editieren
#teil ausf?hren, speichern
#datei im neuen project wieder ?ffnen und weiter ausf?hren

#_________________________________
#create package----

#define package name and directory
pkg_path <- file.path("~","Zellkraftwerk","packages","RJobTissueArea")
pkg_name <- "RJobTissueArea"
#pkg_path <- "C:/Users/Ortmann/Documents/Zellkraftwerk/packages/CANOPY/RJobDistanceMetric"


#directory----
dir.create(pkg_path,recursive = TRUE)
pkg_path<-normalizePath(pkg_path)

usethis::create_package(pkg_path)


#__________________
#active project----

usethis::proj_sitrep() #
usethis::proj_get() # gets actual path for usethis
usethis::proj_set(".") # set this to new project and activates project
usethis::proj_sitrep() #



#_____________________
#description file----

usethis::use_description(fields=list(
  `Authors@R` = c(
    person("Julia", "Ortmann", email = "julia.ortmann@bruker.com", role = c("aut","cre"))
  ),
  License = "file LICENSE",
  Language =  "ger",
  Title = "RJobTissueArea",
  Description = "Determination of chip area covered by tissue in selected positions. Functions for input data generations are copied from the project FDR (updated Version, including dependencie bugfix)",
  Version = "0.0.0.0"),
  check_name = TRUE,
  roxygen = TRUE)

#__________________________
#license----

#add license
usethis::use_mit_license("file LICENCE")

# edit Licence files
# -> LICENSE.md
#____
# License
#
#Copyright (c) 2022 Zellkraftwerk
#
#PROPRIETARY
#Do not distribute outside Zellkraftwerk a BRUKER Company

# -> LICENSE
#______
#YEAR: 2021
#COPYRIGHT HOLDER: Zellkraftwerk GmbH a BRUKER Company
#



#______________
#.Renviron----
usethis::edit_r_environ("project")

#add to file
_R_CHECK_DONTTEST_EXAMPLES_=FALSE

#__________________
#git and github----

usethis::proj_set()
#usethis::git_sitrep()
#usethis::git_vaccinate()
usethis::use_git()
usethis::git_sitrep() # for checking settings
usethis::use_github(private=TRUE)

#_______________
#README----
usethis::use_readme_rmd()
# edit RMD
# build Readme


#logo----

usethis::use_logo("C:/Users/Ortmann/Documents/Zellkraftwerk/templates/Logo/logo.png")
# add line to README
# RJobDistanceMetricOctober <img src='man/figures/logo.png' align="right" height="120" />
usethis::use_lifecycle_badge("experimental")
devtools::build_readme()


#_____________
#news----
usethis::use_news_md()
#edit NEWS
#- Each change should be included in a bulleted list. If you have a lot of changes you might want to break them up using subheadings, ## Major changes, ## Bug fixes etc. I usually stick with a simple list until just before releasing the package when I'll reorganise into sections, if needed. It's hard to know in advance exactly what sections you'll need.
#- If an item is related to an issue in GitHub, include the issue number in parentheses, e.g. (#???10). If an item is related to a pull request, include the pull request number and the author, e.g. (#???101, @hadley). Doing this makes it easy to navigate to the relevant issues on GitHub.
#- The main challenge with NEWS.md is getting into the habit of noting a change as you make a change.

#______________
#.Rprofile----
usethis::edit_r_profile("project")

# add to profile:
usethis::proj_set()
utils::file.edit("NEWS.md")

#___________
#devtools----
usethis::use_devtools()
# add to profile:
if (interactive()) {
  suppressMessages(require(devtools))
}

#_________
#pipes----
usethis::use_pipe()

#____________
#add packages
usethis::use_package("rmarkdown")
usethis::use_package("usethis")
usethis::use_package("devtools")
usethis::use_package("reprex")
usethis::use_package("conflicted")
usethis::use_package("logger")
usethis::use_package("data.table")
usethis::use_package("pkgnet")

usethis::use_package("R.utils")
usethis::use_package("XML")
usethis::use_package("assertive")
usethis::use_package("dplyr")
usethis::use_package("lubridate")
usethis::use_package("mongolite",min_version=TRUE)
usethis::use_package("openssl")
usethis::use_package("params")
usethis::use_package("purrr")
usethis::use_package("qs")
usethis::use_package("readxl")
usethis::use_package("rlang")
usethis::use_package("stringr")
usethis::use_package("tictoc")
usethis::use_package("tidyr")
usethis::use_package("xml2")
usethis::use_package("testthat")



#names <- c("chip_ID",
#           "segment_ID",
#           "Tag",
#           "Type",
#           "output_dir",
#           "ObjRef_Gate",
#           ".data",
#           "data",
#           "segments",
#           "status",
#           "lastchange",
#           "filling_rule",
#           "subtask")
#

devtools::document()
#_________
#namespace----
usethis::use_namespace()

#___________
#package doc----
usethis::use_package_doc()
#edit






#check----
#usethis::use_github_action_check_standard()
#von:
#https://github.com/r-lib/actions/blob/master/examples/README.md
#usethis::use_github_action("render-rmarkdown")

#usethis::use_version("major")

#partial warnings----
#usethis::use_partial_warnings()

#usethis::use_test()
#usethis::use_testthat()
#
#usethis::use_vignette()
#usethis::use_tibble()
#usethis::use_roxygen_md()
#usethis::use_rmarkdown_template()

#redep
#usethis::use_revdep()
#revdepcheck::revdep_check(num_workers = 4)
#reprex----
usethis::use_reprex()
#conflicted----
usethis::use_conflicted()
#
#usethis::use_release_issue("3.0.0")



#usethis::create_github_token()
#gitcreds::gitcreds_set()

#usethis::use_pkgdown()
#usethis::use_pkgdown_github_pages()


#usethis::use_data()
#usethis::use_data_raw()
#usethis::use_data_table()

#_____________
#build package----
devtools::document()
devtools::check()
devtools::build()

#vignettes

usethis::use_vignette("HowToUse")
#_____________
#dependency report----
#add a pkgnet dependency report as vignette

library(pkgnet)
#pkg_path <- "C:/Users/Ortmann/Documents/Zellkraftwerk/packages/CANOPY/RJobDistanceMetric"
#pkg_name <- "RJobDistanceMetric"


CreatePackageVignette(
  pkg = ".",
  pkg_reporters = list(DependencyReporter$new(),
                       FunctionReporter$new(),
                       SummaryReporter$new()),
  vignette_path = file.path(pkg_path, "vignettes", "pkgnet-report.Rmd"))



#save this script----
rstudioapi::documentSave(rstudioapi::getActiveDocumentContext())

#copy this script
Rscript <- rstudioapi::getActiveDocumentContext()
nameThisRScript <- basename(Rscript$path)#"initialize_package_project.R"
pathThisRScript <- file.path(pkg_path,"inst","config")
dir.create(pathThisRScript,recursive = TRUE)

file.copy(Rscript$path,
          file.path(pathThisRScript,nameThisRScript))

