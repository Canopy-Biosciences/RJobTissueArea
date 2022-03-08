V <- "080322"
helpers <- "RJob_execution"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- check_if_chip_data_exist()",
    "- check_if_gatefile_exist()",
    "- check.if.valid()",
    "- check_imported_file_lists()",
    "- check_input_files()",
    "- collect_segments_metadata()",
    "- connect_mongoDB()",
    "- copy_file_positionCSV()",
    "- create_cellsCSV_filepath()",
    "- create_flvalues_allGate_filename()",
    "- create_gatecsv_filepath()",
    "- create_valuesCSV_filepath()",
    "- create_MethodHistory_from_EDL()",
    "- create_XML_filename()",
    "- create_working_directory()",
    "- determine_scan_position()",
    "- execute_mainscript_FDR()",
    "- export_full_input()",
    "- extract_files_to_process()",
    "- extract_gates_to_process()",
    "- extract_jobtype()",
    "- extract_chip_IDs()",
    "- extract_gate_names()",
    "- extract_gate_metadata()",
    "- extract_information_from_RService_XML()",
    "- extract_paths_of_fl_files()",
    "- extract_paths_of_gates()",
    "- find_server_path()",
    "- find_chip_path()",
    "- get_channelID_from_EDL()",
    "- get_EDL_from_query_result()",
    "- get_gate_ID_of_AllGate()",
    "- get_sampleType_from_MethodHistory()",
    "- get_segment_ID_of_chipID()",
    "- get_valid_chip_IDs()",
    "- import_customer_form()",
    "- import_R_Service_XML_file()",
    "- import_single_fl_file()",
    "- import_all_fl_files()",
    "- import_single_gate_file()",
    "- import_all_gate_files()",
    "- import_ZKW_rawdata_to_flvalues()",
    "- load_mainscript_output()",
    "- merge_fl_and_gate_files()",
    "- merge_all_input_files_to_one_list_and_export()",
    "- remove_comp()",
    "- return_segment_metadata()",
    "- select_segment_ID()",
    "- set_attrs_full_input()",
    "- stop_if_fatal()",
    "- tidy_up_XML_file_content()",
    "- query_mongoDB()",
    "- query_segment_ID()",
    "- query_channel_limslager()",
    "- write_lines_task()",
    "- find_RService_XML_on_imageserver()",
    "- handle_trycache_error()"
    ))




check_if_chip_data_exist <- function(chip_IDs){

  V <- 080322
  #- added purrr::

  server_path <- find_server_path()$server_path

  chip_path <- purrr::map_chr(chip_IDs,
                       ~find_chip_path(.x,server_path))

  pathExist <- purrr::map_lgl(chip_path,~str_detect(.x,"error_",negate=TRUE))

  pathError <- purrr::map_lgl(chip_path,~str_detect(.x,"error_"))

  writeLines(c("- found no data for chip_ID: ",
               paste0("  + ",chip_IDs[pathError])))

  if(length(which(pathExist==TRUE))>0){
    chip_IDs <- chip_IDs[pathExist]
    writeLines(c("- data for of chips found: ",
                 paste0("  + ",chip_IDs)))
    return(chip_IDs)
  }else(writeLines("!!! no chip_path's returned"))

}

check_if_gatefile_exist <- function(chip_path,segment_ID,gate_ID){

  path <-  create_gatecsv_filepath(chip_path, segment_ID, gate_ID)
  file.exists(path)


}

#' checks and rapairs the structure of FL.csv files
#'
#' @param input.data list of input dataframes
#' @param chip.IDs character of the chip_ID
#' @importFrom rlang :=
#' @keywords internal
#' @export
#' @family RJob mainscript (ZKWapp)
check.if.valid <- function(input.data, chip.IDs, jobtype) {
  # input.data <- content.temp
  # chip.IDs<-chip.ID
  # dir.name <- filesToProcess


  # input.data <- content.temp
  # chip.IDs <- chip.ID
  # jobtype <- jobtype

  # version <- "180717" -original helpers from Anja - updates:
  Version <- 260621
  #-jobtype added as variable
  Version <- 080322
  #- added logger::log_info


  # version <- paste0(version, ", helpers ", version.helpers)
  # writeLines(paste0("Starting helpers script, Version ", version.helpers))

  ## __________________________________________________
  ## Prueft die Struktur einer FL-CSV und bereinigt sie.
  ##
  ## ** Argumente: **
  ## - data.frame mit Inhalt einer FL-CSV
  ## - ID des dazugehoerigen Chips
  ##
  ## ** Rueckgabe: **
  ## Eine Liste mit folgendem Inhalt:
  ## - Bereinigter Inhalt der FL-CSV
  ## - Originalbezeichnungen der Marker
  ## - Markernamen von Nullspalten
  ## _________________________________________________

  current.chip <- chip.IDs

  ## Does the CSV have the expected structure?
  valid <- (ncol(input.data) > 5) &
    (all(names(input.data)[1:5] == c("CellID", "Position", "XPos", "YPos", "Size")))

  ## Sanitize CSV
  if (valid) {
    data.table::setkey(input.data, Position)
    null.cols <- c()
    null.markers <- c()
    valid.numbers <- all(sapply(input.data[, -c(1:5), with = FALSE], is.numeric))
    if (!valid.numbers) {
      fl.cols <- input.data[, -c(1:5), with = FALSE]
      invalid.markers <- colnames(fl.cols)[!sapply(fl.cols, is.numeric)]
      warn_text <- paste("  + Chip", current.chip, "contains invalid values for marker",
                         invalid.markers, "which will be deleted.",
                         sep = " "
      )
      logger::log_warn("{warn_text}")
      temp <- data.table::copy(input.data[, (invalid.markers), with = FALSE])
      invalid.rows <- input.data[which(is.na(apply(temp, 1, function(x) {
        sum(as.numeric(x))
      })))]
      warn_text <- paste0("Invalid FL file rows: ", invalid.rows)
      logger::log_warn("{warning_text}")
      input.data[, (invalid.markers) := NULL]
      null.markers <- invalid.markers
    }
    input.data <- input.data[stats::complete.cases(input.data), ]
    input.data <- input.data[rowSums(input.data[, -c(1:5), with = FALSE]) != 0, ]
    if (nrow(input.data) <= 1) {
      error_text <- paste0("The selected CSV file doesn't contain valid fluorescence values.
                  Chip ID: ", current.chip)
      stop_if_fatal(error_text)
    }

    if (any(colSums(input.data) == 0)) {
      null.cols <- which(colSums(input.data) == 0)
      notnull.cols <- which(colSums(input.data) != 0)
      null.markers <- c(names(input.data)[null.cols], null.markers)
      if (!jobtype %in% c("SummaryQC-SingleChip", "QC_FL_SingleMarker-SingleChip")) {
        input.data <- input.data[, notnull.cols, with = FALSE]
        info_text <- paste0(
          "Column(s) containing only 0's were deleted.",
          "   Marker names: ", null.markers,
          ", Chip ID: ", current.chip
        )
        logger::log_info("{info_text}")
      }
    }
  } else {
    error_text <- paste0("The selected CSV file is not a valid input file. Chip ID: ", current.chip)
    stop_if_fatal(error_text)
  }

  ## Are the marker names valid column names?
  ## If not, save the original marker names and create
  ## syntactically valid column names.

  data.table::setnames(input.data, stringr::str_trim(colnames(input.data))) # Trim whitespace from start and end of string
  valid.names <- make.names(colnames(input.data), unique = TRUE)
  orig.cols <- data.table::copy(colnames(input.data))
  data.table::setnames(input.data, valid.names)
  debug_text <- paste0("Names of input data: ", names(input.data))
  logger::log_debug("{debug_text}")


  #   if (!all(colnames(input.data) == valid.names)){
  #     orig.cols <- colnames(input.data)
  #     data.table::setnames(input.data, valid.names)
  #   } else orig.cols <- colnames(input.data)

  if (length(grep(";", orig.cols)) != 0) {
    error_text <- paste0("Marker names containing semicolons are not allowed. Chip ID: ", current.chip)
    stop_if_fatal(error_text)
  }

  return(list(input.data = input.data, orig.cols = orig.cols, null.markers = null.markers))
} # END check.if.valid

#' Check whether input files are valid
#'
#' This function checks whether at least one fluorescence file and gate files
#' are present. Empty files are removed.
#'
#' @param filesToProcess List of \code{.csv} files with fluorescence values,
#'   tidied up by \code{\link{tidy_up_XML_file_content}}
#' @param gatesToProcess List of \code{.csv} files with cells contained with
#'   gates, tidied up by \code{\link{tidy_up_XML_file_content}}
#'
#' @return List with \code{filesToProcess} without empty or not existing files,
#'   and flag whether fatal errors occurred

#' Check the imported file lists
#'
#' @param fl_file_names List of FL file names
#' @param gatenames List of gate file names
check_imported_file_lists <- function(fl_file_names, gatenames) {
  if (length(unique(fl_file_names)) != length(fl_file_names)) {
    logger::log_warn("The provided fluorescence files are not unique.")
  }

  if (!all(stringr::str_extract(gatenames, "M[0-9]*") %in% fl_file_names)) {
    error_text <- "At least one gate refers to a segmentation not provided
    in this XML file!"
    stop_if_fatal(error_text)
  }
}


check_input_files <- function(filesToProcess, gatesToProcess) {
  fatal <- FALSE

  if (length(filesToProcess) == 0) {
    stop_if_fatal("Input files not specified.")
  }

  if (any(nchar(filesToProcess) == 0)) {
    empty_files <- paste(filesToProcess[-which(nchar(filesToProcess) == 0)], sep = "; ")
    filesToProcess <- filesToProcess[-which(nchar(filesToProcess) == 0)]
    if (length(filesToProcess) == 0) {
      error_text <- "All input fluorescence files are empty."
      stop_if_fatal(error_text)
    } else {
      logger::log_warn("Some fluorescence value files are empty and will be ignored.
                The following files are affected: {empty_files}.")
    }
  }

  if (!all(file.exists(filesToProcess))) {
    absent <- which(!file.exists(filesToProcess))
    files.absent <- filesToProcess[absent]
    absent.text <- paste0(
      "The following data input files do not exist: ",
      paste(files.absent, collapse = "; "), "."
    )
    stop_if_fatal(absent.text)
  }

  if (!all(file.exists(gatesToProcess))) {
    absent <- which(!file.exists(gatesToProcess))
    files.absent <- gatesToProcess[absent]
    absent.text <- paste0(
      "The following gate input files do not exist: ",
      paste(files.absent, collapse = "; "), "."
    )
    stop_if_fatal(absent.text)
  }

  return(filesToProcess)
}

### **collect_segments_metadata()**

#- takes a chip_path (defined by server volume and chip_ID)
#- creates the segment_path
#- lists all segments folders in segment_path
#- determines when each folder was last modified (on disk)
#- calls query_segment_status(), which extracts the mongoDB Metho-Status and its last-change value
#
#' Title
#'
#' description
#'
#' details
#'
#' @param chip_path path to chip folder on image server
#' @return df
#' @export
#'
#' @examples
#' chip_path <- c("\\\\intern.chipcytometry.com\\imagedata\\leipzig_volume0\\M986054")
#' collect_segments_metadata(chip_path)


collect_segments_metadata <- function(chip_path){

  V <- 120221 #initial version
  #___________________________
  segment_folder = file.path(chip_path,"Segments")

  df <- data.frame(
    segment_ID = list.files(segment_folder,pattern = "^E"))

  df <- df%>%
    dplyr::mutate(mtime = file.mtime(file.path(segment_folder,segment_ID))
    )



  status <- query_segment_ID(df$segment_ID)

  df <- dplyr::left_join(df,status,by=c("segment_ID"="UID"))

  # last_modiefied_segment_ID <- df%>%
  #   dplyr::filter(status %in% c("Edited","Finished"))%>%
  #   dplyr::arrange(desc(lastchange))%>%
  #   dplyr::slice(1)%>%
  #   dplyr::pull(segment_ID)%>%
  #   as.character()

  return(df)
}


## chunk "function_connect_mongoDB"{
#' @title connect_mongoDB()
#'
#' @description This Function connects you to collection of the ZKW mongoDB and returns a mongoDB client.
#'
#' @details It connects to the database collection and returns an interface with functions. With these functions the content of the database can be accessed and extracted or changed.
#' # Available functions are: aggregate, count, distinct, drop, export, find, import, index, info, insert, iterate,
#' mapreduce, remove, rename, update.
#'
#' The ZKW mongoDB consists of two main collections  "limslager" or "limsproc". Limslager stores metadata for real-world-objects and limsproc contains methods which were applied.
#'
#' The data storage is organized hierarchically and the relationship between the objects and methods is realized by ID assignment.
#' The mongoDB client is build using the package mongolite 1.0. In newer versions, changes have been made that equire a newer mongoDB version than the one installed in the ZKW infrastructure.
#'
#' You can easily install the required mongolite versions using remotes::install_version("mongolite","1.0")
#' connect_mongoDB() serves as a helperfunction for function query_mongoDB().
#'
#' @param mongo_collection a character of the collection name ("limslager" oder "limsproc")
#' @param mongo_user character of username (chipcytouser)
#' @keywords internal
#' @return mongo_connection: a client to mongo_collection, providing functions
#' @family database related

#' @export
#' @examples
#' \donttest{
#' connect_mongoDB("limslager")
#' }
connect_mongoDB <- function(mongo_collection, # "limslager" oder "limsproc"
                            mongo_user = "chipcytouser") {

  # Version 181020
  # - found in the package development folder: FunctionsRJobDistanceMetric
  # Version 150621
  # - roxygenized package dependencys
  # Version 080322
  # - removed @ importFrom
  # ___________________________
  # objects
  # check input, define output

  mongo_connection <- NULL

  error_check <- try(params::check_args(select = c(mongo_collection, mongo_user)), silent = TRUE)

  if (inherits(error_check, "try-error")) {
    cat(paste0("error_missing input: ", "\n", error_check[1], " - process interupted !!!"))
  } else {

    # _____________________________
    # load config file
    # contains path to the ZKW configuration file
    # finds file
    # reads in
    # extracts variables

    # load
    ZKW_config_file <- paste0(Sys.getenv("COMMONPROGRAMFILES(X86)"), "\\ZELLKRAFTWERK\\ZKW.config")

    # 2a) externen pointer auf die config XML laden
    parsed_url <- XML::xmlTreeParse(file = paste0(ZKW_config_file), error = function(...) {}, useInternalNodes = T)

    # 2b) notwendige Informationen in Variablen laden

    database <- XML::xpathSApply(
      doc = parsed_url,
      path = "//DATABASE",
      XML::xmlValue
    )
    server <- XML::xpathSApply(
      doc = parsed_url,
      path = "//SERVER",
      XML::xmlValue
    )
    sitekey <- XML::xpathSApply(
      doc = parsed_url,
      path = "//SITEKEY",
      XML::xmlValue
    )
    sitetag <- XML::xpathSApply(
      doc = parsed_url,
      path = "//SITETAG",
      XML::xmlValue
    )
    mongo_user <- mongo_user

    # _________________
    # connect mongo-DB
    # mongoDB-URL erstellen und damit zur mongoDB verbinden

    # 3a) sitekey hashen (2049 mit sha 256)

    sitekey_256 <- vector(length = 2049)
    sitekey_256[1] <- openssl::sha256(c(sitekey),
                                      key = NULL
    )
    for (i in 2:2049) {
      sitekey_256[i] <- openssl::sha256(sitekey_256[i - 1],
                                        key = NULL
      )
    }
    sitekey_256[2049]

    # 3b)  resultierende mongoDB-URL

    mongo_url <- paste0("mongodb://", mongo_user, ":", sitekey_256[2049], "@", server, "/", database)

    # 3c) mit mongoDB-Collection verbinden
    mongo_connection <- try(mongolite::mongo(
      collection = mongo_collection,
      db = database,
      url = mongo_url
    ),
    silent = TRUE
    )
    if (inherits(mongo_connection, "try-error")) {
      cat(paste0("error_failed to connect to mongoDB: ", "\n", mongo_connection[1], " - process interupted !!!"))
    }
  }
  return(mongo_connection)
}
## }


#' copy_file_positionCSV()
#'
#' copys the position.csv into output_dir
#'
#' @param xml_dir external pointer to the content of the RService-XML-file
#' @param chip_ID character of the chip_ID
#' @param output_dir character of path to the output
#' @keywords internal
#' @family RJob mainscript
#' @export
copy_file_positionCSV <- function(xml_dir, chip_ID, output_dir) {
  paths_of_fl_files <- extract_paths_of_fl_files(xml_dir)
  logger::log_debug("Paths of fl files: {paths_of_fl_files}")

  positions_path <- stringr::str_remove(
    paths_of_fl_files,
    paste0(chip_ID, ".*")
  ) %>%
    unique()

  positions_path <- gsub("\\\\", "/", positions_path)
  source_file_path <- file.path(positions_path, chip_ID, "positions.csv")
  logger::log_debug("Positions path: {source_file_path}")

  if (file.exists(source_file_path)) {
    logger::log_debug("Assertion on position path passed.")
  } else {
    error_text <- paste0("Cannot find position file: ", source_file_path)
    stop_if_fatal(error_text)
  }


  # PositionCSV <- data.table::fread(input = positions_path, header = TRUE)

  file.copy(
    from = source_file_path,
    to = file.path(output_dir, "positions.csv"),
    copy.date = TRUE
  )
}

create_cellsCSV_filepath <- function(chip_path,segment_ID){
  filepath <- file.path(chip_path,"Segments",segment_ID,"cells.csv")
  return(filepath)
}

create_flvalues_allGate_filename <- function(output_dir, chip_ID,segment_ID){
  file<- paste0(chip_ID,"_flvaluesAllGate_",segment_ID,".csv")
  filepath <- file.path(output_dir, file)
  return(filepath)
}

create_gatecsv_filepath <- function(chip_path,segment_ID,gate_ID){
  V <- 120222 #intial version
  #__________________________
  gate_filepath <- file.path(chip_path,"Segments",segment_ID,"Gates",paste0(gate_ID,".csv"))
  return(gate_filepath)
}

create_valuesCSV_filepath <- function(chip_path,segment_ID){
  filepath <- file.path(chip_path,"Segments",segment_ID,"values.csv")
  return(filepath)
}

create_MethodHistory_from_EDL<-function(EDL){

  # Version taken from Rmd report:
  # "!_final_Markdown_collect_FLvalues_allStains_180920_withCode_COMPLEMENT.Rmd"
  V <- 210921

  if((stringr::str_detect(EDL,"error_"))==TRUE){
    return(EDL)
  }else{

    output<-try(EDL%>%
                  xml2::read_xml()%>%
                  xml2::xml_child("MethodHistory")%>%
                  xml2::xml_children()%>%
                  purrr::map(xml2::xml_attrs)%>%
                  purrr::map_df(~as.list(.)),
                silent = TRUE)

    if(inherits(output,'try-error')==TRUE){
      return(Error=("error_no_MethodHistory in EDLchannel"))
    }
    else
    {
      return(output)
    }
  }}



#' creates the filename of on RService-XML file
#'
#' @param RJob_ID character
#'
#' @return string of the filename
#' @export
#' @keywords internal
#' @family RJob mainscript
#'
#' @examples
#' create_XML_filename("test")
#' create_XML_filename(NA)
#' create_XML_filename(NULL)
create_XML_filename <- function(RJob_ID) {
  paste0(RJob_ID, "_parameters.xml")
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

determine_scan_position <- function(MethodHistory){
  # !!! adapted from get_Metadata_from_EDL()
  #     Version taken from Rmd report:
  # "!_final_Markdown_collect_FLvalues_allStains_180920_withCode_COMPLEMENT.Rmd"

  Error <- character()

  if("Tag" %in% colnames(MethodHistory)==FALSE){
    Error=c(Error,paste0("error_no Tag metadata"))
  }

  if(any(!is.na(MethodHistory$Tag))==FALSE){
    Error=c(Error,paste0("error_no data for Tag, BG or bleaching"))
  }

  MethodHistory<-MethodHistory%>%
    dplyr::filter(!is.na(Tag))

  if((length(MethodHistory$Tag))<1){
    Error=c(Error,paste0("error_no data listed in Tag"))
  }
  n_scans<-dim(MethodHistory)[1]
  MethodHistory$ScanPosition=3:(n_scans+2)
  MethodHistory$n_scans<-n_scans

  MethodHistory<-MethodHistory%>%
    dplyr::filter(!Tag=="*")%>%
    dplyr::filter(!Tag == "BG")

  if(length(MethodHistory$Tag)<1){
    Error=c(Error,paste0("error_scans with a stain listed in Tag column"))
  }

  MethodHistory$Error <- paste0(Error,collapse=", ")
  return(MethodHistory%>%
           dplyr::select("Scan_UID"="UID",Type,Tag,ScanPosition))

}

####execute_mainscript_FDR####
execute_mainscript_FDR <- function(chip_ID,markernames,RJob_ID,jobtype){
  #Version <- "150222" initial version
  Version <- 230222
  # bugfix
  Version <- 080322
  #- added logger::log_info, logger::log_debug()
  #___________________________________________
  logger::log_debug("Version {version}.")

  tictoc::tic("execute mainscript for FDR RJob")



  flvalues <- gates <- input <- list()

  server_path <- find_server_path()
  #chip_path <- map_chr(chip_IDs,~find_chip_path(.x))
  chip_path <- find_chip_path(chip_ID)

  if(dir.exists(chip_path)){

    #segment_IDs <- map_chr(chip_IDs,~get_segment_ID_of_chipID(.x))
    segment_ID <- get_segment_ID_of_chipID(chip_ID)

    if(dir.exists(file.path(chip_path,"Segments",segment_ID))){

      # gate_ID <- map2_chr(chip_IDs, segment_IDs,
      #                    ~get_gate_ID_of_AllGate(.x,.y))

      gate_ID <- get_gate_ID_of_AllGate(chip_ID,segment_ID)

      flvalues <- import_ZKW_rawdata_to_flvalues(chip_path,chip_ID,segment_ID,gate_ID,markernames)
      filepath_flvaluesAll <- create_flvalues_allGate_filename(output_dir,chip_ID,segment_ID)
      data.table::fwrite(flvalues, filepath_flvaluesAll)
      DataFile <- filepath_flvaluesAll

      StorageCSV <- create_gatecsv_filepath(chip_path,segment_ID,gate_ID)

      XML_file_content <- list(
        "Job_UID" = RJob_ID,
        "jobtype" = jobtype,
        "chip_IDs" = as.character(chip_ID),
        "filesToProcess" = DataFile,
        "gatesToProcess" = StorageCSV,
        "gatenames" = "All"
      )

      tidy_XML_file_content <- tidy_up_XML_file_content(XML_file_content)
      #print(tidy_XML_file_content)

      tidy_XML_file_content[["filesToProcess"]] <- check_input_files(
        tidy_XML_file_content[["filesToProcess"]],
        tidy_XML_file_content[["gatesToProcess"]]
      )

      fl_files <- import_all_fl_files(
        all_file_paths = tidy_XML_file_content[["filesToProcess"]],
        chip_IDs = tidy_XML_file_content[["chip_IDs"]],
        jobtype = tidy_XML_file_content[["jobtype"]]
      )

      gate_files <- list()
      if (length(tidy_XML_file_content[["gatesToProcess"]]) >= 1) {
        gate_files <- import_all_gate_files(
          all_file_paths = tidy_XML_file_content[["gatesToProcess"]],
          gatenames = tidy_XML_file_content[["gatenames"]],
          chip_ID = chip_ID
        )
      } else {
        logger::log_info("No gates specified. Using all cells.")
      }

      check_imported_file_lists(names(fl_files), names(gate_files))
      tidy_XML_file_content[["chip_IDs"]] <- names(fl_files)

      full_input <- merge_all_input_files_to_one_list_and_export(
        gate_files = gate_files,
        fl_files = fl_files,
        XML_file_content = tidy_XML_file_content,
        output_dir = output_dir,
        chip_ID = chip_ID,
        RJob_ID = RJob_ID
      )


    }else{return(paste0("error_no Segments folder: ",segment_ID))}


  }else{return(paste0("error_no chip folder: ",chip_ID))}


  #copy_file_positionCSV(xml_dir, chip_ID, output_dir)
  #
  time <- tictoc::toc(quiet = TRUE)
  elapsed_time <- paste0(round(time$toc - time$tic, digits = 1), " sec")
  logger::log_debug("Execute main script for FDR finished {time$msg}.")
  logger::log_debug("Elapsed time: {elapsed_time}")
  logger::log_debug("Executing version: {version}")
}


#' Export entire data set
#'
#' @param all_files List of all relevant files with metadata as attributes
#' @param fl_files List of FL file content
#' @param gate_files List of gate file content
#' @param chip_ID Chip UId
#' @param RJob_ID R Job UID
#' @param output_dir Output directory
export_full_input <- function(all_files, fl_files, gate_files, chip_ID, RJob_ID, output_dir) {
  Version <- 080322
  #- added logger::log_debug

  path_all_files <- file.path(output_dir, paste0("input_", chip_ID, "_", RJob_ID, ".qs"))
  path_fl_files <- file.path(output_dir, paste0("flvalues_", chip_ID, "_", RJob_ID, ".qs"))
  path_gate_files <- file.path(output_dir, paste0("gate_", chip_ID, "_", RJob_ID, ".qs"))

  qs::qsave(all_files, path_all_files)
  logger::log_debug("Exported the following file: {path_all_files}.")

  qs::qsave(fl_files, path_fl_files)
  logger::log_debug("Exported the following file: {path_fl_files}.")

  qs::qsave(gate_files, path_gate_files)
  logger::log_debug("Exported the following file: {path_gate_files}.")
}




# _________________

#' Extract the names of the files that are to be processed from an XML
#'
#' @param xml_dir Directory of the XML file
#' @keywords internal
#' @return Names of the files to be processed
#' @export
#' @family database related
extract_files_to_process <- function(xml_dir) {

  Version <- 080322
  #- added logger::log_debug()


  filesToProcess <- XML::xpathSApply(
    doc = xml_dir,
    path = "//Input/DataFile[@Format = 'csv']",
    fun = XML::xmlValue
  )
  logger::log_debug("File path for processing: {filesToProcess")
  return(filesToProcess)
}

#' Extract the names of the gates that are to be processed from an XML
#'
#' @param xml_dir Directory of the XML file
#' @keywords internal
#' @return Names of the gates to be processed
#' @export
#' @family database related
extract_gates_to_process <- function(xml_dir) {

  Version <- 080322
  #- added logger__log_debug()


  gatesToProcess <- XML::xpathSApply(
    doc = xml_dir,
    path = "//Input/Object[@Type = 'Gate']/SpecificParameter[@Name = 'StorageCSV']",
    fun = XML::xmlValue
  )
  logger::log_debug("Gate path for processing: {gatesToProcess}")
  return(gatesToProcess)
}


#' Extract the job type that is to be executed
#'
#' @param xml_dir Directory of the XML file
#' @keywords internal
#' @return Name of the job type
#' @export
#' @family database related
extract_jobtype <- function(xml_dir) {

  Version <- 080322
  #- added logger::log_debug()


  jobtype <- XML::xpathSApply(
    doc = xml_dir,
    path = "//Job",
    fun = XML::xmlGetAttr,
    name = "Type"
  )
  logger::log_debug("Jobtype: {jobtype}")
  return(jobtype)
}

#' Extract the chip IDs to be used during job execution
#'
#' @param xml_dir Directory of the XML file
#' @keywords internal
#' @return UIDs of the chip(s)
#' @export
#' @family database related
extract_chip_IDs <- function(xml_dir) {
  Version <- 080322
  #- added logger::log_debug()


  chip_IDs <- XML::xpathSApply(
    doc = xml_dir,
    path = "//Input/Object[@Type = 'Gate']/SpecificParameter[@Name = 'Parentchannel']",
    fun = XML::xmlValue
  )
  channel_ids <- paste0(unique(chip_IDs), collapse = ",")
  logger::log_debug("ChannelIDs of the gates: {channel_ids}")
  return(chip_IDs)
}

#' Extract the names of the gates used for the job
#'
#' @param xml_dir Directory of the XML file
#'
#' @return Gate names
#' @export
#' @family database related
#' @keywords internal
extract_gate_names <- function(xml_dir) {
  Version <- 080322
  #- added logger::log_debug()


  gatenames <- XML::xpathSApply(
    doc = xml_dir,
    path = "//Input/Object[@Type = 'Gate']",
    fun = XML::xmlGetAttr,
    name = "Name"
  )
  logger::log_debug("Gate name: {gatenames}")
  return(gatenames)
}




extract_gate_metadata <- function(chip_IDs){

  #V120222 - initial version
  #_____________________________
  V <- "230222"
  #- extraction of AllObjRefs out of result$result$EDL
  #- bugfix
  v<- "080322"
  #- added purrr::, tidyr::

  df <- data.frame(chip_ID = chip_IDs)

  result <- query_mongoDB("limslager",
                          "UID",
                          chip_IDs)

  df <- df%>%
    dplyr::mutate(ObjRef_Gate = purrr::map(result$result,
                                    ~.x$EDL%>%
                                      xml2::read_xml()%>%
                                      xml2::xml_find_all("/Obj/EncapsulatedObjectsRef/ObjRef[@Type = 'Gate']")%>%
                                      purrr::map(xml2::xml_attrs)%>%
                                      purrr::map_df(~as.list(.))))%>%
    tidyr::unnest(cols = c(ObjRef_Gate))

  result2 <- query_mongoDB("limslager",
                           "UID",
                           df$UID)

  df <- df%>%
    dplyr::mutate(Ref_Name = purrr::map_chr(result2$result,
                                     ~.x$EDLName),
                  ParentSegmentation = purrr::map_chr(result2$result,
                                               ~.x$EDL%>%
                                                 xml2::read_xml()%>%
                                                 xml2::xml_find_all("//*/SpecificParameter[@Name='Parentsegmentation']")%>%
                                                 xml2::xml_attr("Value")))
  #
  #df <- data.frame(chip_ID = as.character(chip_IDs))%>%
  #  mutate(allObjRefs = purrr::map(result$result,
  #                                 ~.x$allObjectReferences%>%unlist()))%>%
  #  unnest(cols = c(allObjRefs))
  #
  #result2 <- query_mongoDB("limslager",
  #                         "UID",
  #                         df$allObjRefs)

  return(df)

}



#' Get relevant information from the input XML
#'
#' @param xml_dir Path to the parsed XML
#'
#' @return Relevant information: Job UID, job type, all file and gate names
extract_information_from_RService_XML <- function(xml_dir) {
  UID <- XML::xpathSApply(xml_dir, "//Job", XML::xmlGetAttr, "UID")
  jobtype <- XML::xpathSApply(xml_dir, "//Job", XML::xmlGetAttr, "Type")
  chip_IDs <- extract_chip_IDs(xml_dir)
  gatesToProcess <- extract_paths_of_gates(xml_dir)
  filesToProcess <- extract_paths_of_fl_files(xml_dir)
  gatenames <- extract_gate_names(xml_dir)

  return(list(
    "Job_UID" = UID,
    "jobtype" = jobtype,
    "chip_IDs" = chip_IDs,
    "filesToProcess" = filesToProcess,
    "gatesToProcess" = gatesToProcess,
    "gatenames" = gatenames
  ))
}

#' Extract the paths of FL value files used for this job
#'
#' @param xml_dir Directory of the XML file
#'
#' @return File Paths of the FL value files
extract_paths_of_fl_files <- function(xml_dir) {

  Version <- 080322
  #- added logger::log_debug()


  file_paths <- XML::xpathSApply(
    doc = xml_dir,
    path = "//Input/DataFile[@Format = 'csv']",
    fun = XML::xmlValue
  )

  file_paths <- purrr::map_chr(
    file_paths,
    ~ if (R.utils::isAbsolutePath(.x)) {
      .x
    } else {
      file.path(".", .x)
    }
  )
  logger::log_debug("Input fl file: {file_paths}.")
  return(file_paths)
}



#' Extract the paths of the gate files used for the job
#'
#' @param xml_dir Directory of the XML file
#'
#' @return File paths for the gate files
extract_paths_of_gates <- function(xml_dir) {
  Version <- 080322
  #- added logger::log_debug()


  gate_paths <- XML::xpathSApply(
    doc = xml_dir,
    path = "//Input/Object[@Type = 'Gate']/SpecificParameter[@Name = 'StorageCSV']",
    fun = XML::xmlValue
  )
  gate_paths <- purrr::map_chr(
    gate_paths,
    ~ if (R.utils::isAbsolutePath(.x)) {
      .x
    } else {
      file.path(".", .x)
    }
  )
  logger::log_debug("Input gate file: {gate_paths}.")
  return(gate_paths)
}


#' finds available server storing images generated by ZKWapp
#'
#' @export
#' @family database related
#' @keywords internal
#' @examples
#' \donttest{
#' find_server_path()
#' }
find_server_path <- function() {
  # check which Servers are available at a site, on which images can be stored
  error <- NULL
  # V150621s
  # stored in the directory' C:\Users\Ortmann\Documents\Zellkraftwerk\R_reports\autogating\ValuesCsvsOfScanWithCutoffCD4/R
  # filename: function_find_server_path
  # were also stored within this project for further development
  # and a copy of the final/actual Version for general usage to the in folder: C:\Users\Ortmann\Documents\Zellkraftwerk\Rscripts_versionining

  # updates:
  # a functions name used, changed to query_mongoDB
  # Version 080322
  # - removed @ importFrom

  # server_names<-query_mongoDB(search_value="ImageServerPath",
  #                            mongo_collection = "limslager",
  #                            search_object = "EDLType",
  #                            return_columns = c("FlagEmpty","EDLName","EDL"))%>% #,"Do_Not_Check" gibts gar nicht
  #  dplyr::filter(FlagEmpty == 0)
  #- roxygenize package dependencie

  names_result <- query_mongoDB(
    mongo_collection = "limslager",
    attribute_name = "EDLType",
    attribute_value = "ImageServerPath"
  )

  error <- names_result$error_message
  if (rlang::is_empty(error)) {
    server_names <- names_result$result[[1]]
    server_names <- server_names %>%
      dplyr::select(.data$FlagEmpty, .data$EDLName, .data$EDL) %>%
      dplyr::filter(.data$FlagEmpty == 0)

    # find server path in the EDL
    server_names <- server_names %>%
      dplyr::mutate(server_path = purrr::map_chr(
        .data$EDL,
        ~ .x %>%
          xml2::read_xml() %>%
          xml2::xml_find_all("//SpecificParameter[@Name='StoragePath']") %>%
          xml2::xml_attr("Value")
      )) %>%
      dplyr::select(-.data$EDL)
  } else {
    error_text <- "Cannot find server names."
    stop_if_fatal(error_text)
  }


  return(server_names)
}


#' searches for a chip_ID
#'
#' finds the folder path to a related chip_ID in a list of server paths
#'
#' @param ChannelID chracter if the chip_ID
#' @param server_path vector containing characters of server paths
#' @keywords internal
#' @export find_chip_path
#'
#' @examples
#' \donttest{
#'
#' server <- find_server_path()
#' chip_path <- find_chip_path("'M1153570", server$server_path)
#' }
find_chip_path <- function(ChannelID = "M583054",
                           server_path = NULL) {
  # V021020
  # stored in the directory' C:\Users\Ortmann\Documents\Zellkraftwerk\R_reports\autogating\ValuesCsvsOfScanWithCutoffCD4/R
  # filename: function_find_chip_path
  # also stored for development in folder: C:\Users\Ortmann\Documents\Zellkraftwerk\Rscripts_versionining

  ### check input
  if (is.null(server_path)) {
    server_path <- find_server_path()$server_path
  }

  if (any(c(
    !is.character(ChannelID),
    !is.vector(server_path),
    length(ChannelID) != 1
  ))) {
    error_text <- paste0("Cannot find chip path: ", ChannelID)
    stop_if_fatal(error_text)
  } else {
    if (!any(file.exists(server_path))) {
      return("error_server do not exist under the provided address")
    } else {
      if (any(stringr::str_detect(ChannelID, "error_"))) {
        return(ChannelID)
      } else {

        ### create data objects
        chip_path <- NA
        check_file <- FALSE
        n_server <- length(server_path)
        i <- 1
        # i=0

        ### loop through all server paths and check if chip  exists

        while ((!check_file) & i <= n_server) {

          # repeat {
          # i=i+1
          chip_path <- paste0(server_path[i], "\\", ChannelID) %>% file.path()
          check_file <- file.exists(chip_path)
          i <- i + 1

          # if (check_file | i >= n_server) break
        }

        ### return chip_path
        # or error message depending on existance of the path

        if (check_file==FALSE) {

          error_text <- "error_no path to chip data found on the provided servers."
          logger::log_debug(paste0(error_text, ": ","{chip_path}"))
          chip_path <- error_text

          #stop_if_fatal(error_text)
        }else{

          logger::log_debug("Successfully found chip path: {chip_path}")

        }
        return(chip_path)
      }
    }
  }
} # }#}



#' finds the file path to RJob-xml
#'
#' querry mongoDB for the path to a given chipID in the list of available serverpaths and creates a final string to the folder where RJob XML can be found
#' @param chipID a character of chipID
#' @param segmentID  a charceter of segmentID
#' @param RJobID a character of RJobID
#' @keywords internal
#' @family RJob mainscript
#' @export
#' @return lists list of server_path's, chip_path, XML_path
#'
#' @examples
#' \donttest{
#' #need access to mongoDB
#' # chip in Leipzig
#' chipID <- "M186808"
#' segementID = "EZKL110986"
#' RJobID <- "EZKL113783"
#'
#' find_RService_XML_on_imageserver(chipID,segementID,RJobID)
#' }
find_RService_XML_on_imageserver<- function(chipID,
                                            segmentID,
                                            RJobID){
  functions <- "find_RService_XML_on_imageserver"
  V<- "091121"
  #- removed XML-file naming, return is onle XML_filepath (execute_mainscript will add RJob_parameter.xml to path)
  #V091121
  #- initial version, outsourced from execute_mainscript V260621

  filling_rule("",pad="- ")
  filling_rule(paste0("Start - ",functions,", Version ",V ),"...")
  filling_rule(" ",pad=".")

  tryCatch({

    #______________________
    #1) find_server_path
    #______________________
    task <- "find_server_path-"

    server <- find_server_path()

  }, error = function(err) {handle_trycache_error(err,paste0(task),enable.quit)})
  write_lines_task(task,NULL,"E")

  tryCatch({

    #______________________
    #2__find_chip_path
    #______________________

    task <- "find_chip_path-"

    chip_path <- find_chip_path(ChannelID = chipID,
                                server_path = server$server_path)

  }, error = function(err) {handle_trycache_error(err,paste0(subtask),enable.quit)})
  write_lines_task(task,NULL,"E")

  #________________________________
  #3) create_string_of_RJOB_XMLpath
  #________________________________

  tryCatch({

    task <- "create_string_of_RJOB_XMLpath"

    filepath_RJob_EDL <- file.path(chip_path,
                                   "Segments",segmentID,
                                   RJobID,
                                   create_XML_filename(RJobID))
  }, error = function(err) {handle_trycache_error(err,paste0(subtask),enable.quit)})
  write_lines_task(task,NULL,"E")

  return_list <- list(
    server_path = server$server_path,
    chip_path=chip_path,
    filepath_RJob_EDL = filepath_RJob_EDL)


  filling_rule(paste0("End execution - ",functions,", Version ",V ))
  filling_rule("- ")

  return(return_list)
}


get_channelID_from_EDL<-function(EDL){
  chipUID <-EDL%>%
    xml2::read_xml()%>%
    xml2::xml_find_first("Identity")%>%
    xml2::xml_attr("UID")
  return(chipUID)
}
#

get_EDL_from_query_result <- function(result){

  V <- 130222 # initial Version
  V <- 080322
  #- added purrr::
  #____________________________

  return <- purrr::map_chr(result$result,
                    ~ .x$EDL)
}

get_gate_ID_of_AllGate<- function(chip_ID,segment_ID){
  V<-"230222"
  # changes IDs$chip_ID to IDs$UID
  # changes "IDs$allObjRefs" to IDs$UID
  # bugfix
  pos <- integer(0)
  IDs <- extract_gate_metadata(chip_IDs = chip_ID)
  pos<-which(IDs$chip_ID == chip_ID &
               IDs$ParentSegmentation == segment_ID&
               IDs$Ref_Name == "All")

  gate_ID <- IDs$UID[pos]

  if(length(pos)>0){
    return(gate_ID)
  }else{stop("found no All in ObjRef names")}

}


get_sampleType_from_MethodHistory<-function(MethodHistory){

  V <- 080322
  #- added stats::

  if(any(MethodHistory%>%
         purrr::simplify()%>%
         stats::na.exclude()%>%
         stringr::str_detect("error_")==TRUE)){
    if((is.character(MethodHistory) & length(MethodHistory)==1)){
      return(MethodHistory)
    }else{return("error_no MethodHistory")}

  }else{

    Type=MethodHistory$Type

    sampleType<-dplyr::case_when(any(stringr::str_detect(Type%>%na.exclude(), "tissue"))~"tissue",
                                 any(stringr::str_detect(Type%>%na.exclude(),"cellsolution")) ~ "cellsolution",
                                 TRUE ~ "error_sampleType not found")
    return(sampleType)
  }
}

get_segment_ID_of_chipID <- function(chip_ID){
  segment_ID <- character(0)
  segment_ID <- chip_ID%>%
    as.character()%>%
    return_segment_metadata()%>%
    select_segment_ID()


  if(length(segment_ID)==1){
    return(segment_ID)

  }else{stop("failed getting segment_ID chip_ID")}
}


get_valid_chip_IDs <- function(chipCSV,
                               ColumnsToSelect,
                               chipColumn){
  v <- 080322
  #- added stats::, dplyr::pull::

  customer_form <- import_customer_form(chipCSV,
                                        ColumnsToSelect)

  chip_IDs <- customer_form%>%
    dplyr::pull(chipColumn)%>%
    stats::na.exclude()

  chip_IDs <- check_if_chip_data_exist(chip_IDs)

  return(chip_IDs)

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
#' @export
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


import_customer_form <- function(chipCSV,
                                 ColumnsToSelect,
                                 header_row=3){

  customer_form <- readxl::read_excel(chipCSV,
                                      skip = header_row - 1)%>%
    dplyr::select_at(ColumnsToSelect)

  return(customer_form)
}


#' Import XML file containing job description and file paths
#'
#' @param XML_filepath Path to the XML file
#' @param output_dir Output directory for the parsed XML file
#' @param RJob_ID R Job UID
#' @param chip_ID Chip UID
#' @param segment_ID Segmentation job UID
#'
#' @return File path of the parsed XML
import_R_Service_XML_file <- function(XML_filepath, output_dir, RJob_ID, chip_ID, segment_ID) {
  if (rlang::is_null(XML_filepath)) {
    XML_filepath <- find_RService_XML_on_imageserver(
      chip_ID,
      segment_ID,
      RJob_ID
    )$filepath_RJob_EDL
  }

  if (!file.exists(XML_filepath) | suppressWarnings(!assertive::is_readable_file(XML_filepath))) {
    stop_if_fatal("Cannot find or read RService XML (looking at {XML_filepath}).")
  }

  xml_dir <- tryCatch(XML::xmlInternalTreeParse(file.path(XML_filepath)),
                      error = function(e) {
                        error_text <- "Cannot parse XML file."
                        stop_if_fatal(error_text)
                      }
  )


  if ("XMLInternalDocument" %in% class(xml_dir)) {
    XML_filename <- create_XML_filename(RJob_ID)
    XML::saveXML(xml_dir, file.path(output_dir, XML_filename))
    logger::log_debug("RService-XML file: {XML_filename}.")
    logger::log_debug("XML file location: {output_dir}.")
  } else {
    stop_if_fatal("Invalid XML file.")
  }

  return(xml_dir)
}






#' Import one .csv file
#'
#' @param file_path Valid path to the file
#' @param csv_type Type of the file, e.g. "FLCSV"
#' @param chip_ID UID of the chip to which this file belongs
#' @param jobtype Type of the job that is being executed
#'
#' @return A tidy \code{.csv} file imported as \code{data.table}
import_single_fl_file <- function(file_path, csv_type, chip_ID, jobtype) {
  content_temp <- data.table::fread(file_path)
  fl_file <- check.if.valid(content_temp, chip_ID, jobtype)
  logger::log_debug("Successfully imported fl file {file_path}.")
  return(fl_file)
}

#' Import multiple .csv files
#'
#' @param all_file_paths Valid paths to all files that are to be imported
#' @param csv_type Type of the files
#' @param chip_IDs UIDs of the chips to which the listed files belong
#' @param jobtype Type of the job that is to be executed
#'
#' @return List with all \code{.csv} files imported as \code{data.table}
import_all_fl_files <- function(all_file_paths,
                                csv_type = "FLCSV", chip_IDs, jobtype) {
  if (!(length(all_file_paths == length(chip_IDs)))) {
    error_text <- "Number of files is not the same as number of chip IDs."
    stop_if_fatal(error_text)
  }

  all_fl_files <- mapply(import_single_fl_file,
                         all_file_paths, csv_type, chip_IDs, jobtype,
                         SIMPLIFY = FALSE
  )
  names(all_fl_files) <- chip_IDs

  logger::log_success("All fl value files successfully imported.")

  return(all_fl_files)
}

#' Import a single gate file
#'
#' @param file_path A valid file path
#'
#' @return Gate file as \code{data.table}
import_single_gate_file <- function(file_path) {
  gate_file <- data.table::fread(file_path)

  if (ncol(gate_file) != 2) {
    stop_if_fatal("Gate file not valid!")
  }

  data.table::setnames(gate_file, c("Position", "CellID"))
  logger::log_debug("Successfully imported gate file {file_path}.")
  return(gate_file)
}

#' Import a list of gate files
#'
#' @param all_file_paths Valid file paths
#' @param gatenames Names of the gates
#' @param chip_ID Chip UID
#'
#' @return List of gate files as \code{data.table}s
import_all_gate_files <- function(all_file_paths, gatenames, chip_ID) {
  if (!(length(all_file_paths == length(gatenames)))) {
    error_text <- "Number of gate files is not the same as number of gatenames."
    stop_if_fatal(error_text)
  }

  all_gate_files <- lapply(all_file_paths, import_single_gate_file)
  # all_gate_files <- list()
  # for(i in 1:length(all_file_paths)){
  #   all_gate_files[[i]] <- import_single_gate_file(all_file_paths[[i]])
  # }
  names(all_gate_files) <- paste0(chip_ID, "_", gatenames)
  logger::log_success("All gate files successfully imported.")
  return(all_gate_files)
}

import_ZKW_rawdata_to_flvalues <- function(chip_path,chip_ID,segment_ID,gate_ID,markernames="CD4"){

  #V120222 #intial version
  V <- 230222
  # - apply make.unique on colnames of values_subset
  #   ...right join of cells and gateAll failed if marker was measured more than once, dupblicated colnmaes, replace join by logical filtering vector defined using which
  # - adding chip_ID extraction from chip_path
  # - bugfix
  V <- 080322
  #- added dplyr::
  #__________________________

  #chip_ID <- basename(chip_path)

  filepath_cellsCSV <- create_cellsCSV_filepath(chip_path,segment_ID)
  filepath_valuesCSV <- create_valuesCSV_filepath(chip_path,segment_ID)
  filepath_AllGate <-  create_gatecsv_filepath(chip_path,segment_ID,gate_ID)

  values <- data.table::fread(filepath_valuesCSV,fill=TRUE)%>%
    dplyr::rename("CellID"="cell",
           "Position"="position")

  cells <- data.table::fread(filepath_cellsCSV,fill=TRUE)%>%
    dplyr::select("CellID"="cell",
                  "Position"="position",
                  "XPos"="xpix",
                  "YPos"="ypix",
                  "Size"="radiuspix")
  gateAll <- data.table::fread(filepath_AllGate,fill=TRUE)

  chip_ll <- query_mongoDB("limslager","UID",chip_ID)
  MethodHistory <- create_MethodHistory_from_EDL(chip_ll$result[[1]]$EDL)
  ScanPosition <- determine_scan_position(MethodHistory)

  if(all(markernames %in% ScanPosition$Tag)){
    pos <- c(which(ScanPosition$Tag == markernames))
    colpos <- c(1,2,ScanPosition$ScanPosition[pos])
    colname <- c("Position","CellID",ScanPosition$Tag[pos])

    values_subset <- values[,colpos, with=FALSE]
    colnames(values_subset)<-colname%>%make.unique(sep="_")

    flvalues<-dplyr::right_join(
      cells,
      gateAll,
      by=c("Position"="V1","CellID"="V2"))%>%
      dplyr::left_join(values_subset,
                       by = c("Position", "CellID"))

    #pos <- which(values_subset$Position %in% gateAll$V1) & which(values_subset$CellID %in% gateAll$V2)
    #flvales <- cells[pos,]

    return(flvalues)

  }else{stop("markernames do not match Tags of Scans - check markernames")}
}



# _________________________
# load_mainscript_output()
# _________________________

#' loads all objects generated by the mainscript
#'
#' loads objects present in the Renvironment of ZKWapp after mainscript
#' exceution, run after execute_RJob_mainscript() to load all the objects
#'
#' input, flvalues, gates, xml.dir, , UID, jobtype, gatenames, chip.IDs
#' and in addition PositionCSV and chip_path are loaded into the
#' user-R-environment
#'
#' @param chip_ID character
#' @param RJob_ID character
#' @param input.dir character defining directory of the data to load, if set to
#'  null standard is subdirectory data/chip_ID/mainscript
#' @export
#' @family RJob mainscript
#' @keywords MainScript
#' @examples
#' \donttest{
#' chip_ID <- "M1153570"
#' segment_ID <- "E1549294"
#' RJob_ID <- "E1568014"
#'
#' # execute_RJob_mainscript(chip_ID,segment_ID,RJob_ID,NULL)
#' load_mainscript_output(chip_ID, RJob_ID, NULL)
#' }
load_mainscript_output <- function(chip_ID, RJob_ID, input.dir = NULL) {
  task <- "load mainscript output"

  # V260621 - updates:
  # - include gates and flvalues
  # - referencing includes RJob_ID
  # - input.dir definition option
  # - chip_path and PisitionCSV added
  Version <- 150222
  #updates:
  #- adapted for FDR RJob, no PositionCSV and xml
  # --
  # chip_ID <- "M936044"
  # segment_ID <- "E1306403"
  # RJob_ID <- "E1568081"


  # directory

  ## ___________________
  ## _2 value in output_dir
  # subtask <- "_value_in_output_dir"

  # input.dir <- check_envir_variable("input.dir",
  #                                   file.path("data",chip_ID,"mainscript"))
  # write_lines_task(task,subtask,"E")
  # working.dir <- getwd()

  # input.dir <-  file.path(working.dir,input.dir)

  # ___________________
  # _2 directory
  subtask <- "_in_working.dir"

  if (!R.utils::isAbsolutePath(input.dir)) {
    working.dir <- getwd()

    input.dir <- file.path(working.dir, input.dir)

    write_lines_task(task, subtask, "E")
  }


  # filename
  if (!all(c(
    input.dir %>% file.path(paste0("input_", chip_ID, "_", RJob_ID, ".qs")) %>% file.exists(),
    input.dir %>% file.path(paste0("flvalues_", chip_ID, "_", RJob_ID, ".qs")) %>% file.exists(),
    input.dir %>% file.path(paste0("gate_", chip_ID, "_", RJob_ID, ".qs")) %>% file.exists()#,
    #input.dir %>% file.path(file.path("positions.csv")) %>% file.exists()
  ))) {
    error_text <- paste0("Cannot find ZKW raw data in the input directory: ", input.dir)
    stop_if_fatal(error_text)
  }


  input_file <- file.path(input.dir, paste0("input_", chip_ID, "_", RJob_ID, ".qs"))
  flvalues_file <- file.path(input.dir, paste0("flvalues_", chip_ID, "_", RJob_ID, ".qs"))
  gates_file <- file.path(input.dir, paste0("gate_", chip_ID, "_", RJob_ID, ".qs"))


  # load input
  input <- qs::qread(input_file)
  flvalues <- qs::qread(flvalues_file)
  gates <- qs::qread(gates_file)


  # extract attributes
  # input.dir <- attr(input,"output_dir")
  UID <- attr(input, "UID")
  jobtype <- attr(input, "jobtype")
  gatenames <- attr(input, "gatenames")
  chip.IDs <- attr(input, "chip.IDs")
  xml.name <- attr(input, "ServiceEDL")
  chip_path <- attr(input, "chip_path")

  # parse xml
  #xml.dir <- XML::xmlInternalTreeParse(file.path(input.dir, xml.name))

  # load positions.csv

  #PositionCSV <- data.table::fread(file.path(input.dir, "positions.csv"))

  # list of variables
  var_list <- list(
    input = input,
    flvalues = flvalues,
    gates = gates,
    #PositionCSV = PositionCSV,
    #xml.dir = xml.dir,
    output_dir = input.dir,
    UID = UID,
    jobtype = jobtype,
    gatenames = gatenames,
    chip.IDs = chip.IDs,
    chip_path = chip_path
  )

  return(var_list)
}


#' Merge chip fl value file and corresponding gate files
#'
#' @param gate_files All gate files from the current job
#' @param curr_fl_file The fl value file of a chip
#' @param chip_ID The chip UID
#'
#' @return A list containing one element for each gate that corresponds to this
#'   chip UID. Each list element is itself a list element containing the FL
#'   values of all the chips within this gate and further information as
#'   produced by \code{\link{import_all_fl_files}}.
merge_fl_and_gate_files <- function(gate_files, curr_fl_file, chip_ID) {
  curr_chip <- list()

  # Welche "gates"-Elemente gehoeren zu diesem Chip?
  gates_of_current_chip <- stringr::str_detect(names(gate_files), chip_ID)

  # Fuer jedes Gate des aktuellen Chips:
  for (index in which(gates_of_current_chip)) {
    curr_gate <- gate_files[[index]]

    # Extrahiere aus der urspruenglichen FL-value-Tabelle diejenigen
    # Zellen, die in diesem Gate enthalten sind, und schreibe sie
    # in die input-Liste
    temp_input <- curr_fl_file
    data.table::setkeyv(temp_input$input.data, cols = c("CellID", "Position"))
    data.table::setkeyv(curr_gate, cols = c("CellID", "Position"))
    temp_input$input.data <- merge(curr_gate, temp_input$input.data)
    curr_chip[[length(curr_chip) + 1]] <- temp_input
    names(curr_chip)[[length(curr_chip)]] <- names(gate_files)[[index]]
  }

  return(curr_chip)
}


#' Combine all input files and meta information to one list and export it
#'
#' @param gate_files All gate files
#' @param fl_files All FL value files
#' @param XML_file_content Content of the R Service XML file containing the Chip and R Job UIDs
#' @param output_dir Output directory
#' @param chip_ID Chip UID
#' @param RJob_ID R Job UId
merge_all_input_files_to_one_list_and_export <- function(gate_files, fl_files, XML_file_content, output_dir, chip_ID, RJob_ID) {
  chip_IDs <- XML_file_content[["chip_IDs"]]

  all_files <- list()

  if (length(gate_files) >= 1) {
    for (i in 1:length(fl_files)) {
      curr_chip_files <- merge_fl_and_gate_files(
        gate_files,
        fl_files[[i]], chip_IDs[[i]]
      )
      all_files <- c(all_files, curr_chip_files)
    }
  } else {
    for (i in 1:length(fl_files)) {
      all_files[[i]] <- fl_files[[i]]
      names(all_files)[[i]] <- paste0(chip_IDs[[i]], "_All")
    }
  }
  all_files <- set_attrs_full_input(all_files, XML_file_content, output_dir, RJob_ID)
  export_full_input(all_files, fl_files, gate_files, chip_ID, RJob_ID, output_dir)

  return(all_files)
}

remove_comp <- function(df){

  n_comp <- dim(df)[1]
  n_slice <- n_comp -1
  if(n_comp>2){
    df <- df%>%
      dplyr::slice(1:n_slice)

  }
  return(df)
}

### **return_segments_metadata()**

#- calls find_chip_path() and collect_segment_metadata() for several chipIDs


return_segment_metadata <- function(chip_IDs){

  V <- 120222 # initial version
  #____________________________

  IDs <- data.frame(
    chip_ID = chip_IDs,
    chip_path = purrr::map_chr(
      chip_IDs,
      ~find_chip_path(.x)))%>%
    tidyr::nest(data = c(chip_path))%>%
    dplyr::mutate(segments = purrr::map(
      data,
      ~collect_segments_metadata(.x$chip_path)))%>%
    tidyr::unnest(cols = c(data, segments))

  return(IDs)
}

##test hannover
#chip_IDs<- "M912067"
##test leipziog
##chip_IDs<- c("M407766","M583196","M986054")
#return_segment_metadata(chip_IDs)
#```
#### **select_segment_ID()**
#
#- takes df containing segment metadata
#- filteres for those segmentations with status c("Edited","Finished")
#- returns the folder which was recently modiefied using the DBentry
#

select_segment_ID<- function(segment_IDs){

  V <- 120222 # initial version
  V <- 080322
  # - added dplyr::desc
  #____________________________

  segment_ID <- segment_IDs %>%
    dplyr::filter(status %in% c("Edited","Finished"))%>%
    dplyr::group_by(chip_ID)%>%
    dplyr::arrange(dplyr::desc(lastchange))%>%
    dplyr::slice(1)%>%
    dplyr::pull(segment_ID)

  return(segment_ID)
}

#' Save job metadata as attributes
#'
#' @param full_input List of all files to be saved
#' @param tidy_XML_file_content Metadata extracted from the R Service XML
#' @param output_dir Output directory
#' @param RJob_ID R Job UID
#'
#' @return \code{full_input} with attributes
set_attrs_full_input <- function(full_input, tidy_XML_file_content, output_dir, RJob_ID) {
  attr(full_input, "ServiceEDL") <- create_XML_filename(RJob_ID)
  attr(full_input, "output_dir") <- output_dir
  attr(full_input, "UID") <- tidy_XML_file_content[["Job_UID"]]
  attr(full_input, "jobtype") <- tidy_XML_file_content[["jobtype"]]
  attr(full_input, "gatenames") <- tidy_XML_file_content[["gatenames"]]
  attr(full_input, "chip_IDs") <- tidy_XML_file_content[["chip_IDs"]]
  return(full_input)
}

#' Stop execution
#'
#' @param message Message to be shown when execution is stopped
stop_if_fatal <- function(message = "Please see chunk output above.") {
  logger::log_fatal("{message}")
  stop(message, call. = FALSE)
}


#' Adapt the XML file content to the package needs
#'
#' For the gate names, "+" is replaced by "plus", "-" is replaced by "minus",
#' punctuation is replaced by underscore and non-unique names get a count.
#' Special characters need to be removed to produce valid file names in later
#' steps.
#'
#' @param XML_file_content All relevant information from the RService XML as
#'   produced by \code{\link{extract_information_from_RService_XML}}
#'
#' @return Tidied XML file content
tidy_up_XML_file_content <- function(XML_file_content) {
  gatesToProcess <- XML_file_content[["gatesToProcess"]]
  chip_IDs <- XML_file_content[["chip_IDs"]]
  gatenames <- XML_file_content[["gatenames"]]

  if (any(grepl("\\+", gatenames))) {
    gatenames <- gsub("\\+", "plus", gatenames)
  }
  if (any(grepl("\\-", gatenames))) {
    gatenames <- gsub("\\-", "minus", gatenames)
  }

  # do not use stringr here as e.g. stringr doesn't recognize "$" as punctuation
  if (any(grepl("[[:punct:]]", gatenames))) {
    gatenames <- gsub("[[:punct:]]", "_", gatenames)
  }
  gatenames <- make.unique(gatenames)
  gatenames <- stringr::str_replace_all(gatenames, "\\.", "_")

  logger::log_debug("Tidied up gate names.")

  # __remove empty gates----

  if (any(gatesToProcess == "")) {
    empty.gates <- which(gatesToProcess == "")
    gatesToProcess <- gatesToProcess[-empty.gates]
    gatenames <- gatenames[-empty.gates]
    chip_IDs <- chip_IDs[-empty.gates]
  }
  logger::log_debug("Removed empty gates.")

  names_gatesToProcess <- paste0(chip_IDs, "_", gatenames)

  chip_IDs <- unique(chip_IDs)

  return(list(
    "Job_UID" = XML_file_content[["Job_UID"]],
    "jobtype" = XML_file_content[["jobtype"]],
    "chip_IDs" = chip_IDs,
    "filesToProcess" = XML_file_content[["filesToProcess"]],
    "gatesToProcess" = gatesToProcess,
    "gatenames" = gatenames
  ))
}




# V <- "150222"
# - initial version - updates
#---
V <- 150222
helpers <- "FDR"

assign(paste0("version.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading function - ", helpers, ", Version ", V))
writeLines("---------------------")





## chunk "function_query_mongoDB"{
#' @title query_mongoDB()
#'
#' @description This Function performs for you a query in a ZKW-mongoDB-collection and tries to find those EDL objects for which a certain attribute matches a value.
#'
#' @details
#'
#' You need to specify
#' 1) which collection you want to look at: "limslager" or "limsprroc" and
#' 2) what you want to look at, meaning the name of an attribute and
#' 3) what you want to find, the value of the attribute.
#'
#' The function calls connect_mongoDB() to provide the external pointer to the DB-collection.
#' It creates a json string of the query-command, performs the query for all json strings and collects the query results.
#' A string containing an "mongo_collection", "attribute_name" and "attribute_value" is added to each results.
#' It further records querries which yielded no result, meaning that there is no entry in the database and
#' creates for those a vercor error messages containing all error messages of the query.
#' All results where combined to a list of the result-list and the error message is returned.
#'
#' @param mongo_collection character, name of the ZKW mongoDB collection, "limslager" or "limsproc"
#' @param attribute_name character, name of the EDL attribute
#' @param attribute_value character, value of the EDL attribute
#' @keywords internal
#' @return list of df: result and vector: error_message
#' @family database related
#' @export
#'
#' @examples
#' \donttest{
#'
#' query_result <- query_mongoDB(
#'   mongo_collection = "limsproc",
#'   attribute_name = "UID",
#'   attribute_value = "M856222"
#' )
#'
#' query_result <- query_mongoDB(
#'   mongo_collection = "limslager",
#'   attribute_name = "EDLType",
#'   attribute_value = c("Antibody Stock Solution", "Antibody Stock Solution")
#' )
#' }
query_mongoDB <- function(mongo_collection,
                          attribute_name,
                          attribute_value) {
  # Version 181020
  # - found in the package development folder: FunctionsRJobDistanceMetric
  # Version 150621
  # - roxygenize package dependency
  # Version 080322
  # - removed @ importFrom

  # define output and working variables
  error <- NULL
  mongo_connection <- NULL
  query_string <- NULL
  query_result <- NULL
  no_result <- NULL
  result <- NULL
  message <- NULL
  error_message <- NULL

  # check input
  error_check <- try(
    params::check_args(select = c(
      mongo_collection,
      attribute_name,
      attribute_value
    )),
    silent = TRUE
  )

  if (inherits(error_check, "try-error")) {
    cat(paste0("error_missing input: ", "\n", error_check[1], " - process interupted !!!"))
  } else {

    # connect to mongoDB-collection
    mongo_connection <- connect_mongoDB(mongo_collection)

    if (!inherits(mongo_connection, "try-error")) {

      # vector of query strings
      query_string <- paste0('\'{\"', attribute_name, '\":\"', attribute_value, '\"}\'')

      # map trough query vector
      query_result <- purrr::map(
        query_string,
        ~ mongo_connection$find(query = eval(parse(text = .x)))
      )

      # seperate query results from query without a result
      no_result <- purrr::map_lgl(
        query_result,
        ~ rlang::is_empty(.x)
      )

      # prepare string for query with result

      result <- query_result[which(no_result == FALSE)]
      message <- paste0(attribute_name, "_", attribute_value, "_", mongo_collection)[which(no_result == FALSE)]

      # append result with the message
      result <- purrr::map2(
        result,
        message,
        ~ .x %>%
          dplyr::mutate(query = .y)
      )

      # prepare a vector with error messages to return
      error_message <- paste0("error_no EDL found in ", mongo_collection, " for ", attribute_name, "_", attribute_value)
      error_message <- error_message[which(no_result)]

      return(list(result = result, error_message = error_message))
    }
  }
}
## }

#' Title
#'
#' @param segment_IDs
#'
#' @return data.frame
#' @export
#'
#' @examples
#' segment_IDs <- c("EZKL122811","EZKL207112","EZKL289562")

query_segment_ID <- function(segment_IDs){

  V <- "120222" #initial version
  #_____________________________

  result <- query_mongoDB("limsproc",
                          "UID",
                          segment_IDs)

  status <- purrr::map_chr(result$result,
                           ~.x$Status)

  UID <- purrr::map_chr(result$result,
                        ~.x$UID)

  lastchange = purrr::map_chr(result$result,
                              ~.x$lastchange%>%
                                as.character()) %>%
    lubridate::ymd_hms()

  if(length(result$error_message)>0){
    writeLines(c("- query error records: ",
                 paste0("  ",result$error)))
  }

  return(data.frame(UID=UID,
                    status=status,
                    lastchange = as.POSIXct(lastchange)))
}



### **collect_segments_metadata()**

#- takes a chip_path (defined by server volume and chip_ID)
#- creates the segment_path
#- lists all segments folders in segment_path
#- determines when each folder was last modified (on disk)
#- calls query_segment_status(), which extracts the mongoDB Metho-Status and its last-change value
#

#' Title
#'
#' @param chip_path
#'
#' @return result
#' @export
#'
#' @examples
#' chip_path <- c("\\\\intern.chipcytometry.com\\imagedata\\leipzig_volume0\\M986054")
#' collect_segments_metadata(chip_path)

#
#```

### **return_segments_metadata()**

#- calls find_chip_path() and collect_segment_metadata() for several chipIDs

#```{r}


#
#```
#
#```{r}
##test hannover
#chip_IDs<- "M912067"
##test leipziog
##chip_IDs<- c("M407766","M583196","M986054")
#return_segment_metadata(chip_IDs)
#```
#### **select_segment_ID()**
#
#- takes df containing segment metadata
#- filteres for those segmentations with status c("Edited","Finished")
#- returns the folder which was recently modiefied using the DBentry
#
#```{r}

#```
#
#### **extract_gate_UIDs()**
#
#```{r}
##chip_IDs<- c("M407766","M583196","M986054")
#segment_IDs <- chip_IDs%>%
#  as.character()%>%
#  return_segment_metadata()%>%
#  select_segment_ID()
#
#IDs <- data.frame(chip_ID = chip_IDs,
#                  segment_ID = segment_IDs)
#
#IDs
#```
#
#```{r}

#```
#
#```{r}
#extract_gate_metadata(IDs$chip_ID)
#```
#```{r}
#IDs <-left_join(IDs,
#                extract_gate_metadata(IDs$chip_ID),
#                by=c("segment_ID"="ParentSegmentation","chip_ID"))
#
#IDs
#```
#


### **query_channel_limslager()**

#```{r}
query_channel_limslager<- function(chip_IDs){

  V <- 130222 # initial Version
  V <- 080322
  #- added return(result)
  #____________________________

  result <- query_mongoDB("limslager",
                          "UID",
                          chip_IDs)

  return(result)
}
#```
#
#```{r}
#chip_ll <- query_channel_limslager(chip_IDs = IDs$chip_ID)
#```
#
#### **get_EDL_from_query_result()**
#
#```{r}


#```
#
#```{r}
#
#EDL <- query_channel_limslager(
#  chip_IDs = IDs$chip_ID)%>%
#  get_EDL_from_query_result()%>%
#  modify_if(is.factor, as.character)
#```
#
#### **create_chip_MethodHistory()**
#
#```{r}
#```
#
#```{r}
#MethodHistory <- map(EDL,~create_MethodHistory_from_EDL(.x))
#```
#
#### **get_sampleType_from_MethodHistory()**
#
#```{r}

#```
#
#
#```{r}
#get_sampleType_from_MethodHistory(MethodHistory = MethodHistory[[1]])
#```
#### **get_channelID_from_EDL()**
#
#
#```{r}

#get_channelID_from_EDL(EDL = EDL[1])
#```
#### **determine_scan_position()**
#
#```{r}


#```
#
#```{r}
#determine_scan_position(MethodHistory = MethodHistory[[1]])
#```
#
#### **get_segment_ID_of_chipID()**
#
#```{r}

#```
#
#### **get_gate_ID_of_AllGate()**
#
#```{r}

#```
#
#### **create_cellsCSV_filepath()**
#
#```{r}

#```

### **create_valuesCSV_filepath()**

#```{r}

#```

### **create_gatesCSV_filepath()**

#```{r}

#```

### **create_flvalues_allGate_filename()**

#```{r}


#```

## Rohdatenimport

#```{r,eval=FALSE}
#chip_ID <- "M407766"
#get_segment_ID_of_chipID(chip_ID)
#get_gate_ID_of_AllGate(chip_ID,segment_ID)
#find_server_path()
#find_chip_path(chip_ID)
#create_cellsCSV_filepath(chip_path,segment_ID)
#create_valuesCSV_filepath(chip_path,segment_ID)
#create_gatecsv_filepath(chip_path,segment_ID,gate_ID)
#create_flvalues_allGate_filename(output.dir,chip_ID,segment_ID)
#```
#
#### **import_ZKW_rawdata_to_flvalues()**
#
#```{r}
#### **import_cellsCSV()**
#


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
#' @export
#' @examples
#' {
#'   write_lines_task("task", type = "S")
#'   write_lines_task("task", "subtask", "E")
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










