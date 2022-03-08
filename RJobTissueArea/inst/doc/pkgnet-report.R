## ----setup, include=FALSE-----------------------------------------------------
library(pkgnet)

knitr::opts_chunk$set(
    echo = FALSE
    , warning=FALSE
    , out.width='100%'
)
pkgnet:::silence_logger()

reporters <- list(DependencyReporter$new(), FunctionReporter$new(), SummaryReporter$new())

reporters <- lapply(
          X = reporters
          , FUN = function(reporter){
              reporter$set_package(pkg_name = "RJobTissueArea")
              return(reporter)
          }
      )


## ----echo = FALSE, error = TRUE-----------------------------------------------
reporter$calculate_default_measures()

## ----echo = FALSE, error = TRUE-----------------------------------------------
reporter$graph_viz

## ----error = TRUE-------------------------------------------------------------
reporter$get_summary_view()

## ----echo = FALSE, error = TRUE-----------------------------------------------
reporter$calculate_default_measures()

## ----echo = FALSE, error = TRUE-----------------------------------------------
reporter$graph_viz

## ----error = TRUE-------------------------------------------------------------
reporter$get_summary_view()

## ----results='asis'-----------------------------------------------------------
reporter$get_summary_view()

## ----warning=FALSE------------------------------------------------------------
reportSections <- lapply(reporters, function(reporter) {
  report_env <- list2env(list(reporter = reporter))
  knitr::knit_child(
    reporter$report_markdown_path
    , envir = report_env
  )
})


## ----results="asis"-----------------------------------------------------------
cat(paste0(paste(reportSections, collapse = '\n')))

## ----results="asis"-----------------------------------------------------------
cat(sprintf("<sup>This report built with **pkgnet v%s**.</sup>", packageVersion('pkgnet')))

## ----echo = FALSE-------------------------------------------------------------
pkgnet:::unsilence_logger()

