V <- "080522"
helpers <- "DBrelated"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_long_node_df_from_XML()",
    "- connect_mongo_DB() - behelfsweise"
    ))

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
    #server <- XML::xpathSApply(
    #  doc = parsed_url,
    #  path = "//SERVER",
    #  XML::xmlValue
    #)
    server <- "mongosrv2.intern.chipcytometry.com:27017"

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

