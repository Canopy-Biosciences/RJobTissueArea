# R/factory.R
#'
#'
#' @param group_ID
#' @param output_dir
#' @param sigma
#' @param threshold
#' @param window
#'
#' @title Tissue Area Analysis Factory
#' @description performs complete workflow of TissueAreaAnalysis
#' @details is targets-factory function which calls the functions supplied with this package and stores and tracks the output
#' Define 6 targets:
#' 1. Track the user-supplied input
#' 2. loads valid chipIDs `load_valid_chipIDs()`
#' 3. generate ScanHistory `generate_ScanHistory()`
#' 4. creates input data `generate_InputImage()`
#' 5. performs Image Processing `process_Images()`
#' 6. calculates TissueArea `calculate_TissueArea()`
#' 7. exports the result `export_TissueAreaResults()`
#' @return A list of target objects.
#' @export
target_factory <- function(group_ID,output_dir,sigma,threshold,window) {
  list(
#    tar_target_raw("file", file, format = "file", deployment = "main"),
#    tar_target_raw("data", quote(read_data(file)), format = "fst_tbl", deployment = "main"),
#    tar_target_raw("model", quote(run_model(data)), format = "qs")
  )
}
