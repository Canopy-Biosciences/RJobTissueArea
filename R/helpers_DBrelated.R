V <- "300622"
helpers <- "DBrelated"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- check_if_chip_data_exist()",
    "- connect_mongo_DB() - behelfsweise",
    "- create_long_node_df_from_XML()",
    "- extract_chipIDs_from_groupEDL()",
    "- find_chip_path()",
    "- find_server_path()",
    "- find_valid_group_chip_IDs",
    "- get_EDL_from_query_result()",
    "- get_enabled_positions()",
    "- get_positions_field_from_query_result()",
    "- query_chipID_channels()",
    "- query_filterset_of_scanID()",
    "- query_UID_limslager()",
    "- query_UID_limsproc()",
    "- query_mongoDB()"
    ))

#' checks if chipID data folder exist
#'
#' Internally query for available Imageserver. Subsequent maps through all chip_IDs and check if a folder named by chip_IDs exist on the Server. Finally seperates the chip_IDs by folder existance and prints them seperatly. Returns only existing data chip_IDs.
#'
#' @param chip_IDs character vector of chip_IDs
#'
#' @return character vector of chip_IDs for which data folder exist
#' @export
#' @keywords internal
#' @family database related
#' @examples
#' \donttest{
#' data(chip_IDs)
#' check_if_chip_data_exist(chip_IDs)
#' }
#'
check_if_chip_data_exist <- function(chip_IDs){

  V <- 080322
  V <- 280522
  #- added purrr::, stringr::
  #- variable binding
  #- checkmate input

  server_path <- chip_path <- pathExist <- pathError <- NULL

  checkmate::assert_character(chip_IDs,
                              any.missing = FALSE,
                              min.len =1)

  server_path <- find_server_path()$server_path

  chip_path <- purrr::map_chr(chip_IDs,
                              ~find_chip_path(.x,server_path))

  pathExist <- purrr::map_lgl(chip_path,~stringr::str_detect(.x,"error_",negate=TRUE))

  pathError <- purrr::map_lgl(chip_path,~stringr::str_detect(.x,"error_"))

  writeLines(c("- found no data for chip_ID: ",
               paste0("  + ",chip_IDs[pathError])))

  if(length(which(pathExist==TRUE))>0){
    chip_IDs <- chip_IDs[pathExist]
    writeLines(c("- data for of chips found: ",
                 paste0("  + ",chip_IDs)))
    return(chip_IDs)
  }else(writeLines("!!! no chip_path's returned"))

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
#' @return mongo_connection: a client to mongo_collection, providing functions
#' @family database related
#' @export
#' @keywords internal
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
  # Version TEMPORÄR 080521
  # - manually input mongo mongo_server: zkw2
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
    #server <- "mongosrv2.intern.chipcytometry.com:27017"

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




#' create_long_node_df_from_XML
#'
#' @param XML
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
create_long_node_df_from_XML <- function(XML){

  all_nodes <- XML %>%
    xml2::xml_find_all(".//*")

  all_nodes <- purrr::map(all_nodes,
                          ~.x)

    df <- data.frame(xml_node = vector(length = length(all_nodes))) %>%
    dplyr::mutate(xml_node = purrr::map(all_nodes, ~.x))

    names(df$xml_node) <- purrr::map_chr(df$xml_node, ~xml2::xml_name(.x))

  df <- df %>% dplyr::mutate(node_name = purrr::map_chr(xml_node,
                                                        ~.x %>% xml2::xml_name()), node_attributes = purrr::map(xml_node,
                                                                                                                ~.x %>% xml2::xml_attrs()), node_path = purrr::map_chr(xml_node,
                                                                                                                                                                       ~.x %>% xml2::xml_path()))
  df_longer <- df %>% tidyr::unnest_longer(node_attributes) %>%
    dplyr::select(node_name, node_attributes, node_attributes_id,
                  node_path, xml_node)%>%
    dplyr::select(-xml_node)%>%
    as.data.frame()

  return(df_longer)
}

#' extracts chipIDs from its chipgroup EDL
#'
#' converts a given string to an XML object and path's over to all ObjRefs and extracts UID attributes.
#'
#' @param EDL string of an XML document
#'
#' @return character vector containing chipIDs
#' @export
#' @keywords internal
#' @family database related
#' @examples
#' {
#' EDL <- "<?xml version=\"1.0\"?>\r\n<!-- Mit XMLSpy v2011 rel. 3 (http://www.altova.com) von Christian Hennig (Medizinische Hochschule Hannover Päd. Pneumologie Neonatologie) bearbeitet -->\r\n
#' <Obj MadewithEDLVersion=\"0.1\" derivedfrom=\"Data Collection\" madewithEDLVersion=\"29012013141654\" Type=\"ChipGroup\">\r\n\t
#' <Identity UID=\"P1761451\" Type=\"ChipGroup\" TakeoverFromInputObject=\"\" Name=\"Takeda - sample preparation\"/>\r\n\t
#' <PrimaryContainer UID=\"\" Type=\"Earth\" TakeoverFromInputObject=\"\" IsInputObject=\"\"/>\r\n\t
#' <SpecificParameters>\r\n\t\t
#' <SpecificParameter Name=\"Definition\" Value=\"A ChipGroup is a collection of sample-analysis-data that shares the same function within a project (e.g. control-group, patient-group)\" Unit=\"Text\"/>\r\n\t\t
#' <SpecificParameter Name=\"DefinitionURL\" Value=\"-\" Unit=\"URL or Database\"/>\r\n\t\t
#' <SpecificParameter Name=\"Lifetime\" Value=\"-\" Unit=\"Duration\"/>\r\n\t\t
#' <SpecificParameter Name=\"ChipGroup Description\" Value=\"\" Unit=\"Text\"/>
#' <SpecificParameter Name=\"ChipGroup URL\" Value=\"\" Unit=\"URL\"/>
#' <SpecificParameter Name=\"ChipGroup Name\" Value=\"Takeda - sample preparation\" Unit=\"Text\"/>
#' </SpecificParameters>\r\n\t<EncapsulatedObjects/>
#' <EncapsulatedObjectsRef>
#' <ObjRef UID=\"M1730408\" Type=\"Microfluidic-Channel for Chipcytometry-Chip1 Histology-FFPE\"/>
#' <ObjRef UID=\"M1730410\" Type=\"Microfluidic-Channel for Chipcytometry-Chip1 Histology-FFPE\"/>
#' <ObjRef UID=\"M1730412\" Type=\"Microfluidic-Channel for Chipcytometry-Chip1 Histology-FFPE\"/>
#' </EncapsulatedObjectsRef>
#' <Owners>
#' <Owner UID=\"H858698\"/>
#' <Owner UID=\"CU1746580\" Level=\"FullAccess\"/>
#' </Owners></Obj>\r\n"
#'
#' EDL%>%
#' extract_chipIDs_from_groupEDL()
#'}
extract_chipIDs_from_groupEDL<- function(EDL){

  chip_IDs <- NULL
  checkmate::check_character(EDL,
                             any.missing = FALSE,
                             len = 1)
  chip_IDs <- EDL%>%
    xml2::read_xml()%>%
    xml2::xml_find_all("/Obj/EncapsulatedObjectsRef/ObjRef")%>%
    xml2::xml_attr("UID")

  return(chip_IDs)
}




#' searches for a chip_ID
#'
#' finds the folder path to a related chip_ID in a list of server paths. If no serverpaths are provided to the function, a internal function is called.
#'
#' @param ChannelID character containing a chip_ID
#' @param server_path vector containing characters of server paths
#' @export
#' @keywords internal
#' @return a character containing a chip_ID string or error message
#' @family database related
#' @examples
#' \donttest{
#' data(chip_IDs)
#' chip_path <- find_chip_path(chip_IDs[1])
#'
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




#' finds available server storing images generated by ZKWapp
#'
#' @export
#' @keywords internal
#' @family database related
#' @return a df containing flag, EDLName and path to the server
#' @family database related
#' @examples
#' \donttest{
#' find_server_path()
#' }
find_server_path <- function() {
  # check which Servers are available at a site, on which images can be stored

    # V150621s
  # stored in the directory' C:\Users\Ortmann\Documents\Zellkraftwerk\R_reports\autogating\ValuesCsvsOfScanWithCutoffCD4/R
  # filename: function_find_server_path
  # were also stored within this project for further development
  # and a copy of the final/actual Version for general usage to the in folder: C:\Users\Ortmann\Documents\Zellkraftwerk\Rscripts_versionining

  # updates:
  # a functions name used, changed to query_mongoDB
  # Version 080322
  # Version 2805222
  # - removed @ importFrom

  # server_names<-query_mongoDB(search_value="ImageServerPath",
  #                            mongo_collection = "limslager",
  #                            search_object = "EDLType",
  #                            return_columns = c("FlagEmpty","EDLName","EDL"))%>% #,"Do_Not_Check" gibts gar nicht
  #  dplyr::filter(FlagEmpty == 0)
  #- roxygenize package dependencie
  #- variable binding

  name_result <- error <- server_names <- error_text <- NULL

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



#' finds valid chipIDs of a chipgroup
#'
#' returns all chipIDs of a given chipgroup for which  a data folder could be found
#'
#' The mongoDB collection limslager is queried for the groupID.
#' From the EDL return object the UID attributes are extracted for all ObjRef's in the EncapsulatedObjectsRef.
#' The UIDs correspond to the chipIDs. Subsequently, all available ImageServers are searched for the chip folders.
#' If no folder could be found for a chipID, this chip is excluded from the result.
#' The IDs of the excluded chips are printed separately to the console and only the remaining chipIDs are returned.
#'
#' @param group_ID character of chip group_ID
#' @return character vector containing chip_IDs for which data is available
#' @export
#' @family database related
#' @examples
#' \dontrun{
#' group_ID <- "P1761451"
#' chip_IDs <- find_valid_group_chip_IDs(group_ID)
#' }
find_valid_group_chip_IDs <- function(group_ID){

  Version <- 270522
  #- UPDATE
  #- variableBinding added
  #- documentation added
  #- logger and tictoc added

  #logging----
  logger::log_debug("Version {version}.")
  tictoc::tic("find valid group chip_IDs")

  #check input----
  checkmate::assert_character(group_ID,
                              min.chars =5,
                              any.missing = FALSE,
                              len = 1)
  #variable binding----
  query_result <- EDL <- chip_IDs <- valid_chipIDs <- NULL

  #find group_ID in limslager----
  query_result <- query_mongoDB("limslager","UID",group_ID)

  #check query result----
  if(checkmate::test_list(query_result$result, len = 1)){

    #extract EDL from result_df----
    EDL <- query_result%>%get_EDL_from_query_result()

    #extract chip_IDs----
    chip_IDs <- EDL%>%extract_chipIDs_from_groupEDL()

    #check if data exist----
    valid_chipIDs <-chip_IDs%>%check_if_chip_data_exist()

    #logger----
    logger::log_success()

    return(valid_chipIDs)

  }else{

    logger::log_error()
    logger::log_debug(query_result$error_message)

    return(query_result$error_message)
  }

  time <- tictoc::toc(quiet = TRUE)
  elapsed_time <- paste0(round(time$toc - time$tic, digits = 1), " sec")
  logger::log_debug("finished ?{time$msg}.")
  logger::log_debug("Elapsed time: {elapsed_time}")
  logger::log_debug("Executing version: {Version}")
}





#' extracts all EDL strings from a mongoDB query result
#'
#' @param result list containing the mongoDB result
#'
#' @return a character vector containing EDL strings
#' @export
#' @keywords internal
#' @family database related
#' @examples
#' query_result <- list(
#' result = list(structure(list(
#' EDL = "<?xml version=\"1.0\"?>\r\n<!-- ",
#' EDLType = "ChipGroup"),
#' query = "UID_P1761451_limslager")),
#' error_message = character(0))
#'
#' EDL <- query_result%>%
#' get_EDL_from_query_result()
#'
get_EDL_from_query_result <- function(result){

  V <- 130222 # initial Version
  V <- 080322
  V<- 280522
  #- added purrr::
  #- docu and data, inputcheck
  #- checkmate input
  #____________________________

  EDL <- NULL




    EDL <- purrr::map_chr(result$result,
                          ~ .x$EDL)
    return(EDL)
}


#' get_enabled_positions
#'
#' @param chip_ID
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
get_enabled_positions <- function(chip_ID){

  query_result<-query_chipID_channels(chip_IDs)
  positions_list <- get_positions_field_from_query_result(query_result)
  enabled_positions <- get_enabled_positions_from_positions_list(positions_list)

  return(enabled_positions)
}


#
#' get_positions_field_from_query_result
#'
#' @param query_result
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
get_positions_field_from_query_result <- function(query_result){

  df <- tibble::tibble(
    chip_ID = purrr::map_chr(query_result$result,
                             ~.x$UID),
    positions = purrr::map(query_result$result,
                           ~.x$positions%>%
                             purrr::flatten()))

  return(df)
}

#' Title
#'
#' @param chip_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
query_chipID_channels <- function(chip_IDs){

  query_result <- query_mongoDB("channels","UID",chip_IDs)
  return(query_result)

}


#' query_filterset_of_scanIDs
#'
#' @param scan_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
query_filterset_of_scanIDs <- function(scan_IDs){

  # query scanIDs in scans
  query_scan_scans <- query_UID_limsproc(scan_IDs)

  # get EDL
  EDL <-get_EDL_from_query_result(query_scan_scans)

  # select node: Active Filterset-ID
  filterset <- purrr::map_chr(EDL,
                              ~.x%>%xml2::read_xml()%>%
                                xml2::xml_find_all('/Method/Machine/SpecificParameters/SpecificParameter[@Name = "Active Filterset-ID"]')%>%
                                xml2::xml_attr("Value"))

  return(filterset)

}

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
#' @return list of df: result and vector: error_message
#' @family database related
#' @export
#' @keywords internal
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
#' @param chip_IDs
#'
#' @return
#' @export
#' @keywords internal
#' @examples
query_UID_limslager<- function(chip_IDs){

  V <- 130222 # initial Version
  V <- 080322
  #- added return(result)
  #____________________________

  result <- query_mongoDB("limslager",
                          "UID",
                          chip_IDs)

  return(result)
}


#' query_UID_limsproc
#'
#' @param chip_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
query_UID_limsproc<- function(chip_IDs){

  V <- 130222 # initial Version
  V <- 080322
  #- added return(result)
  #____________________________

  result <- query_mongoDB("limsproc",
                          "UID",
                          chip_IDs)

  return(result)
}




