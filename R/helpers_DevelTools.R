V <- "260522"
helpers <- "DevelTools"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- add_dependencies_to_DESCRIPTION()",
    "- create_working_directory()",
    "- download_pkg_sources()",
    "- get_package_dependencies()",
    "- handle_trychache_error()",
    "- read_result_file()",
    "- stop_if_fatal()",
    "- write_lines_task()"
  ))

#' find and add all package dependencies to DESCRIPTION file
#'
#'
#'
#' @param exact_version character vector of package names where exact version is obligatory
#' @param ignore_folder character vector of folder names in pkg directory which are build-ignored
#' @param ignore_package character vector of package names to remove from final list
#'
#' @return the DESCRIPTION file is overwritten
#' @export
#' @keywords internal
#'
#' @examples
#' if(FALSE){
#' add_dependencies_to_DESCRIPTION(
#' exact_version = c("mongolite","locfit","EBImage"),
#' ignore_folder <- c("devel"),
#' ignore_package <- "RJobTissueArea"
#'  )
#'  }
add_dependencies_to_DESCRIPTION <- function(exact_version = "mongolite",
                                   ignore_folder = "devel",
                                   ignore_package = "RJobTissueArea"){

  deps_import <- get_package_dependencies(exact_version ,ignore_folder, ignore_package)

  purrr::walk(deps_import$Package,
              ~usethis::use_package(.x,"imports"))
  #add exact version to DESCRIPTION
  purrr::walk2(exact_version,
               rep(TRUE,length(exact_version)),
               ~usethis::use_package(.x,"imports",.y))

}
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

#' Title
#'
#' @param Package
#' @param Version
#' @param lib_path
#'
#' @return
#' @export
#'
#' @examples
#' if(FALSE){
#' dep_pkg <- get_package_dependencies()
#' Package <- dep_pkg$Package
#' Version <- dep_pkg$Version
#' download_pkg_sources(Package,Version)
#' }
download_pkg_sources <- function(Package,
                                 Version,
                                 lib_path = "devel/pkg_sources"){

  CRAN <- "https://cran.rstudio.com/src/contrib"
  #create output directory
  RJobTissueArea:::create_working_directory(lib_path)
  #replace _ in Version
  Version <- stringr::str_replace(Version,"_",".")
  #create package URL
  pkg_Folder <- paste0(Package,"_",Version,".tar.gz")
  URL <- paste0(CRAN,"/",pkg_Folder)
  #download
  e <- purrr::walk(URL,
                   ~try(
                     download.file(
                       url = .x,
                       destfile = paste0(file.path(lib_path,
                                                   basename(.x))))))
  # check what is present
  down_files <- list.files(lib_path)
  pos <- which(!pkg_Folder %in% down_files)
  # select packages to download from archieve
  URL_archive <- paste0(CRAN,"/Archive/",Package[pos],"/",pkg_Folder[pos])

  # then download
  e <- purrr::walk(URL_archive,
                   ~try(
                     download.file(
                       url = .x,
                       destfile = paste0(file.path(lib_path,
                                                   basename(.x))))))

  # check success
  down_files <- list.files(lib_path)

  # list of tar.gz files which needs manual download
  pos <- which(!pkg_Folder %in% down_files)

  writeLines(c("-packages remain to be downloaded manually:",
               Package[pos]))
}

#' create df of package dependencies
#'
#' @param exact_version character vector of package names where exact version is obligatory
#' @param ignore_folder character vector of folder names in pkg directory which are build-ignored
#' @param ignore_package character vector of package names to remove from final list
#'
#' @return
#' @export
#'
#' @examples
#' if(FALSE){
#' get_package_dependencies(
#' exact_version = c("mongolite","locfit","EBImage"),
#' ignore_folder <- c("devel"),
#' ignore_package <- "RJobTissueArea"
#'  )
#'  }
get_package_dependencies <- function(exact_version = "mongolite",
                                     ignore_folder = "devel",
                                     ignore_package = "RJobTissueArea"){
  #get all package dependencies
  deps<-renv::dependencies()
  #get all packages installed
  inst_pkg <- installed.packages()%>%
    as.data.frame()
  #add Installed Version of package dependencies
  deps <- dplyr::left_join(deps,
                           inst_pkg%>%
                             dplyr::select(Package,"InstalledVersion"="Version"),
                           by = "Package")
  #ignore folder
  ignore_pos <- purrr::map(ignore_folder,
                           ~stringr::str_detect(deps$Source,.x)%>%
                             which())%>%
    unlist()
  if(length(ignore_pos)>=1){
    deps <- deps[-ignore_pos,]
  }
  #summarize pkg and version
  deps_import <- deps%>%
    dplyr::group_by(Package)%>%
    dplyr::summarize(Version = unique(InstalledVersion))
  #filter pkg
  deps_import <- deps_import%>%
    dplyr::filter(Package != ignore_package)%>%
    dplyr::mutate(Version = as.character(Version))

  return(deps_import)

}

#' how to proced in terms of an trycache error
#'
#' creates a message string constisting of the error mesage produced in the try chache and its the task defined by the user.
#' Writes the message into the console and into a textfile named by "error_message.txt", located in the output. dir.
#' if \code{enable.quit} is set \code{TRUE} R execution will quit without saving
#'
#' @param err try-error error-function
#' @param task character
#' @param enable.quit logical
#' @return a console print and a txt-file in the output.dir
#' @keywords internal
#' @family RJob execution
#' @examples
#' {

#' output.dir <- getwd()
#' enable.quit<- FALSE
#' task<-"test"
#'
#'testthat::expect_warning(
#' tryCatch({
#' log(-1)
#'  }, error = function(err) {handle_trycache_error(err,task,enable.quit)})
#')
#'testthat::expect_warning(
#' tryCatch({
#' task<-"test_2"
#' log(-1)
#'  }, error = function(err) {handle_trycache_error(err,task,enable.quit)})
#')
#'testthat::expect_warning(
#'  tryCatch({
#' log(-1)
#'  }, error = function(err) {handle_trycache_error(err,task,enable.quit)})
#')
#'
#'  }
handle_trycache_error <- function(err,task,enable.quit=FALSE) {

  V <- 080322
  #- set enable.quit to FALSE (wg globalVariable)

  msg <- c(paste0("- An error occurred while ",task),
           paste0("- ERROR TEXT: ", err))
  writeLines(msg)
  sink(paste0(output.dir, "/error_message.txt"))
  writeLines(msg)
  sink(NULL)

  if (enable.quit){quit(save = "no", status = 666)}else{stop()}
}

#' load a result file and check columns
#'
#' @param result_df
#' @param result_filename
#'
#' @return
#' @export
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

  #______________________
  #get obligatory columns
  cols <- colnames(result_df)
  #____________
  #get dir_name
  result_dir <- dirname(result_filename)
  #___________________
  #check if file exist
  if(file.exists(result_filename)){
    #__________________
    #read and overwrite
    result_df <- readr::read_csv(result_filename)
    #_______________________
    #check obligatory column
    to_add <- cols[which(!cols %in% colnames(result_df))]
    #_________________________
    #check if cols are missing
    if(length(to_add)>=1){
      #______________________________________
      #add missing columns and set content NA
      result_df[,to_add] <- NA
    }
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
#' @keywords internal
stop_if_fatal <- function(message = "Please see chunk output above.") {
  logger::log_fatal("{message}")
  stop(message, call. = FALSE)
}



#' writes user lines of a task
#'
#' writes the print message for Start or Completion of a task, subtask name supplied
#'
#' @param task character of the task
#' @param subtask character of the subtask
#' @param type vcharacter speciefieing if to inform about the start or finish
#'
#' @return a console print produces by writeLines
#' @keywords internal
#' @family programming helpers
#' @examples
#' \donttest{
#' write_lines_task("task", type = "S")
#' write_lines_task("task", "subtask", "E")
#' }
write_lines_task <- function(task, subtask = NULL, type = c("S", "E", "F")) {
  cat_arg <- match.arg(type)

  if (!cat_arg %in% c("S", "E", "TE")) {
    stop("Type not specified")
  }

  if (cat_arg == "S") {
    if (is.null(subtask)) {
      writeLines(c(paste0("- Start task: ", task)))
    } else {
      writeLines(c(paste0("- Start subtask: ", subtask) %>%
                     stringr::str_replace_all("_", " ")))
    }
  }
  if (cat_arg == "E") {
    if (is.null(subtask)) {
      filling_rule("- successful comleted task: ", task %>% stringr::str_replace_all("_", " "), pad = ".")
      filling_rule("", pad = ".")
    } else {
      writeLines(c(paste0("- successful completed subtask: ", subtask) %>%
                     stringr::str_replace_all("_", " ")))
    }
  }
}

