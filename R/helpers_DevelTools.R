V <- "250522"
helpers <- "DevelTools"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_working_directory",
    "- handle_trychache_error()",
    "- stop_if_fatal()",
    "- write_lines_task"
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

