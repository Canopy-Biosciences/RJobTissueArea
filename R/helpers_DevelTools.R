V <- "300622"
helpers <- "DevelTools"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_working_directory()",
    "- read_result_file()",
    "- stop_if_fatal()"
  ))


#' Create working directory if necessary
#'
#' This function checks whether the provided path to the working directory is
#' valid, otherwise a path is constructed based on the current working
#' directory. If necessary, the working directory is created within the file
#' system.
#'
#' @param output_dir Given working directory
#'
#' @return The (existing) output directory
#' @export
create_working_directory <- function(output_dir) {

  Version <- 080322
  #- added logger::log_debug()


  logger::log_debug("Create path for the working directory, if necessary.")

  if (!R.utils::isAbsolutePath(output_dir)) {
    working_dir <- getwd()
    output_dir <- file.path(working_dir, output_dir)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    logger::log_debug("New working directory created.")
  }

  return(output_dir)
}



#' load a result file and check columns
#'
#' @param result_df
#' @param result_filename
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' result_df <- tibble::tibble(
#' group_ID = character(0), # group_ID
#' chip_ID = character(0), #chip_ID
#' pos_ID = numeric(0), #pos_ID
#' sigma = numeric(0),
#' threshold = numeric(0),
#' GS_window = numeric(0),
#' perc_TissueArea = double(0),
#' TissueArea_mm2 = double(0)
#' )
read_result_file <- function(result_df,
                             result_filename){
  #____________
  #get dir_name
  result_dir <- dirname(result_filename)
  #___________________
  #check if file exist
  if(file.exists(result_filename)){
    #__________________
    #read and overwrite
    OLD_result_df <- readr::read_csv(result_filename)

    #_________________
    #combined both dfs
    result_df <- dplyr::bind_rows(result_df,
                                  OLD_result_df)
  }else{
    #_________________
    #create result_dir
    create_working_directory(result_dir)
  }
  #________________
  #return result_df
  return(result_df)
}


#' Stop execution
#'
#' @param message Message to be shown when execution is stopped
#' @export
#' @keywords internal
stop_if_fatal <- function(message = "Please see chunk output above.") {
  logger::log_fatal("{message}")
  stop(message, call. = FALSE)
}

