V <- "270422"
helpers <- "ImportBlobFiles"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_pos_foldername()",
    "- create_Pos_image_filepath()",
    "- create_ScanHistory_of_chipIDs()",
    "- find_scan_basepath()",
    "- get_df_from_query_result()",
    "- list_BlobFileName_in_filepath()",
    "- list_posFolders_in_ScanBasePath()",
    "- query_filterset_of_scanIDs()",
    "- query_UID_limsproc()",
    "- query_UID_scans()",
    "- export_list_all_image_files()",
    "- select_valid_image_files()",
    "- read_BLOB_parameter_from_XML()",
    "- read_image_binary_file()",
    "- extract_encoding_from_blob_parameter()",
    "- convert_binsize_from_encoding()",
    "- extract_image_width_from_blob_parameter()",
    "- extract_image_height_from_blob_parameter()"
  ))

#' convert_binsize_from_encoding
#'
#' @param encoding
#'
#' @return
#' @export
#'
#' @examples
convert_binsize_from_encoding <- function(encoding){
  bin_size <- dplyr::case_when(encoding == "32bit little-endian" ~ 4,
                               encoding == "16bit little-endian" ~ 2)

  return(bin_size)
}

#' create_pos_foldername
#'
#' @param imageServer_path
#' @param basePath
#' @param pos_ID
#'
#' @return
#' @export
#'
#' @examples
create_pos_foldername <- function(imageServer_path,basePath,pos_ID){

  file.path(imageServer_path,basePath,
            paste0("pos",pos_ID))
}



#' create_Pos_image_filepath
#'
#' @param ScanBasePath
#' @param PositionFolder
#' @param Type
#'
#' @return
#' @export
#'
#' @examples
create_Pos_image_filepath <- function(ScanBasePath,
                                      PositionFolder,
                                      Type){
  Type <- match.arg(Type,choices = c("posRef",
                                     "flimages",
                                     "deltaTL",
                                     "focus",
                                     "hdr"))
  image_filepath <- file.path(ScanBasePath,
                              PositionFolder,
                              Type)
  return(image_filepath)


}

#' create_ScanHistory_of_chipIDs
#'
#' @param chip_IDs
#'
#' @return
#' @export
#'
#' @examples
create_ScanHistory_of_chipIDs<-function(chip_IDs){

  MethodHistory <- create_MethodHistory_of_chipIDs(chip_IDs)

  ScanHistorys <- purrr::map(MethodHistory,
                             ~.x%>%
                               dplyr::rename("scan_ID" = "UID")%>%
                               dplyr::rename("cycle_ID" = "CycleUID")%>%
                               tidyr::fill(cycle_ID,  .direction = "up")%>%
                               dplyr::filter(Type == "Chipcytometry-Scan")%>%
                               dplyr::select(scan_ID,cycle_ID,Status,Tag,Excluded,PreparedForDataviz))
  return(ScanHistorys)
}

#' export_list_all_image_files
#'
#' @param chip_IDs
#' @param type
#'
#' @return
#' @export
#'
#' @examples
export_list_all_image_files <- function(chip_IDs,
                                        result_ID,
                                        output_dir){

  # workflow function
  # based on workflow documented in report: "find all blob image files_smarter.Rmd"
  Version <- "250422"

  tictoc::tic("find image files complete workflow")

  #_____________________
  # 1) find server paths----
  server_paths <- find_server_path()
  server_paths <- server_paths$server_path

  #__________________
  # 2) find chip_path----
  chip_paths <- purrr::map_chr(chip_IDs,
                               ~find_chip_path(.x,server_paths))

  #__________________________
  # 3) create imageServer_path----
  imageServer_paths <- purrr::map2_chr(chip_paths,chip_IDs,
                                       ~stringr::str_remove(.x,.y))

  #______________________
  # 4) combine to df: IDs----
  IDs <- dplyr::tibble(
    chip_ID = chip_IDs,
    chip_path = chip_paths,
    imageServer_path = imageServer_paths)

  #______________________
  # 5) create scanHistory----
  ScanHistory = create_ScanHistory_of_chipIDs(chip_IDs)%>%
    dplyr::bind_rows()

  #_________________
  # 6) add filterset----
  ScanHistory <- ScanHistory%>%
    dplyr::mutate(filterset = query_filterset_of_scanIDs(scan_ID))

  #___________________________
  # 7) query chipIDs ins scans----
  query_chip_scans <- query_mongoDB("scans",
                                    "channelUID",
                                    chip_IDs)

  #________________________
  # 8) wrangle query result----
  # 8a) select query columns
  results_chip_scans <- purrr::map(query_chip_scans$result,
                                   ~.x%>%
                                     dplyr::select("chip_ID" = "channelUID",
                                                   "scan_ID" = "UID",
                                                   jobType,
                                                   basePath,
                                                   "Status" = "jobEndState",
                                                   positions,
                                                   `enabled-count`))

  # 8b) subselect positions
  results_chip_scans <- purrr::map(
    results_chip_scans,
    ~.x%>%
      tidyr::unnest(cols="positions")%>%
      dplyr::select(
        dplyr::any_of(c("chip_ID",
                        "scan_ID",
                        "pos_ID" = "posid",
                        "jobType",
                        "basePath",
                        "Status",
                        "chipx",
                        "chipy",
                        "enabled",
                        "bleach-time",
                        "enabled-count"))))

  # 8c) bind rows
  results_chip_scans <- results_chip_scans%>%
    dplyr::bind_rows()

  #_________________________
  # 9) join imageServer_path----
  result_paths <- dplyr::left_join(results_chip_scans,
                                   IDs,
                                   by = "chip_ID")

  #_________________
  # 10) add pos_path----
  result_paths <- result_paths%>%
    dplyr::mutate(pos_path = create_pos_foldername(imageServer_path,
                                                   basePath,
                                                   pos_ID))

  #_________________________
  # 11) add dirs in pos_path----
  result_paths <- result_paths%>%
    dplyr::mutate(image_folder = purrr::map(pos_path,
                                            ~list.dirs(.x,
                                                       full.names = FALSE,
                                                       recursive = FALSE)))%>%
    tidyr::unnest(cols="image_folder")%>%
    dplyr::mutate(image_path = file.path(pos_path,image_folder))

  #_______________________________
  # 12) list files in image_folder----
  result_files <- result_paths%>%
    dplyr::mutate(blob_filename = purrr::map(image_path,
                                             ~list.files(.x)))%>%
    tidyr::unnest(cols="blob_filename")

  #_________________
  # 13) add filetype----
  result_files <- result_files%>%
    dplyr::mutate(filetype = tools::file_ext(blob_filename))

  #_____________________
  # 14) join ScanHistory----
  result_files <- dplyr::left_join(
    result_files,
    ScanHistory,
    by = c("scan_ID", "Status"))

  #___________
  # 15) export----
  # __15a) create name of result file
  file <- create_result_filepath(output_dir,
                                 name_string = "all_image_files_of_groupID",
                                 result_ID,
                                 type = "csv")
  #__15b) export file
  data.table::fwrite(result_files,
                     file)

  #________________________
  # 16) return result_files----
  tictoc::toc()
  return(result_files)
}

#' extract_encoding_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
extract_encoding_from_blob_parameter <- function(blob_parameter){
  encoding <- blob_parameter%>%
    dplyr::filter(node_attributes_id=="encoding")%>%
    dplyr::pull(node_attributes)

  return(encoding)
}

#' extract_image_height_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
extract_image_height_from_blob_parameter <- function(blob_parameter){
  heigth <- blob_parameter%>%
    dplyr::filter(node_name == "size")%>%
    dplyr::filter(node_attributes_id=="height")%>%
    dplyr::pull(node_attributes)%>%
    as.numeric()

  return(height)
}

#' extract_image_width_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
extract_image_width_from_blob_parameter <- function(blob_parameter){

  width <- blob_parameter%>%
    dplyr::filter(node_name == "size")%>%
    dplyr::filter(node_attributes_id=="width")%>%
    dplyr::pull(node_attributes)%>%
    as.numeric()

  return(width)
}
#' find_scan_basepath
#'
#' @param scan_IDs
#'
#' @return
#' @export
#'
#' @examples
find_scan_basepath <- function(scan_IDs){
  query_result <- query_UID_scans(scan_IDs)
  df <- get_df_from_query_result(query_result)
  return(df$basePath)

}



#' get_df_from_query_result
#'
#' @param query_result
#'
#' @return
#' @export
#'
#' @examples
get_df_from_query_result<- function(query_result){

  result <- purrr::map_df(query_result$result, ~.x)

  return(result)

}

#' list_BlobFileName_in_filepath
#'
#' @param image_filepath
#'
#' @return
#' @export
#'
#' @examples
list_BlobFileName_in_filepath <- function(image_filepath){
  Files<-list.files(image_filepath)
  BlobFiles<-Files[stringr::str_detect(Files,".blob32$")]
  BlobFileName<-BlobFiles%>%stringr::str_remove(".blob32$")
  if(length(BlobFileName)==0){
    return(NA)
  }else{return(BlobFileName)}
}

#' list_posFolders_in_ScanBasePath
#'
#' @param ScanBasePath
#'
#' @return
#' @export
#'
#' @examples
list_posFolders_in_ScanBasePath <- function(ScanBasePath){
  positions<-list.files(path=ScanBasePath)
  positions=positions[stringr::str_detect(positions,"pos")]
  return(positions)
}

#' query_filterset_of_scanIDs
#'
#' @param scan_IDs
#'
#' @return
#' @export
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

#' read_image_binary_file
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
read_image_binary_file <- function(blob_parameter) {

  encoding<-extract_encoding_from_blob_parameter(blob_parameter)

  bin_size<-convert_binsize_from_encoding(encoding)

  width <- extract_image_width_from_blob_parameter(blob_parameter)

  height <- extract_image_height_from_blob_parameter(blob_parameter)

  n_pixels<-width * height

  path <- file.path(attr(blob_parameter, "image_path"),
                    attr(blob_parameter, "blob_filename"))

  data<-readBin(path,
                integer(),
                n=n_pixels,
                size=bin_size)

  return (data)
}

#' read_XML_BLOB_parameter
#'
#' @param image_path
#' @param blob_file_name
#' @param blob_filetype
#'
#' @return
#' @export
#'
#' @examples
read_XML_BLOB_parameter<- function(image_path, blob_filename) {

  Version <- "260422"

  path<-file.path(image_path,
              paste0(blob_filename,".xml"))

  XML<-path%>%
    xml2::read_xml()

  XML_parameter <- XML%>%
    create_long_node_df_from_XML()

  attr(XML_parameter,"image_path") <- image_path
  attr(XML_parameter, "blob_filename") <- blob_filename
  attr(XML_parameter,"xml_path") <- path

  #size<-purrr::map_df(
  #  XML,
  #  ~.x%>%
  #  xml2::xml_find_all('/image/size')%>%
  #  xml2::xml_attrs())
#
  #structure<-purrr::map_df(
  #  XML,
  #  ~.x%>%
  #    xml2::xml_find_all('/image/data-structure')%>%
  #    xml2::xml_attrs())
  #
  #statistics <- purrr::map_df(
  #  XML,
  #  ~.x%>%
  #    xml2::xml_find_all('/image/metadata/statistics')%>%
  #    xml2::xml_attrs())
  #
  #extend <- purrr::map_df(
  #  XML,
  #  ~.x%>%
  #    xml2::xml_find_all('/image/metadata/extent')%>%
  #    xml2::xml_attrs())



  return(XML_parameter)
}

#' select_valid_image_files
#'
#' @param result_files
#' @param type
#'
#' @return
#' @export
#'
#' @examples
select_valid_image_files <- function(result_files, type){

  Version <- "250422"

  #_______________
  # 0) check input----
  type <- match.arg(type, choices =c("blob","blob32","png"))

  #_________________________
  # 1) remove excluded scans----
  result_files <- result_files%>%
    dplyr::filter(Excluded %in% c(NA, "FALSE"))%>%
    dplyr::filter(Status == "Finished")

  #_______________________________
  # 2) select image type to return----
  # __2a) .blob----
  if(type == "blob"){
    result_files <- result_files%>%
      dplyr::filter(filetype == "blob")
  }
  # __2b) .blob32----
  if(type == "blob32"){
    result_files <- result_files%>%
      dplyr::filter(filetype == "blob32")
  }
  # __2c) .png----
  if(type == "png"){
    result_files <- result_files%>%
      dplyr::filter(filetype == "png")
  }

  #_________________
  # 3) return result----
  return(result_files)
}

#' query_UID_limsproc
#'
#' @param chip_IDs
#'
#' @return
#' @export
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

#' query_UID_scans
#'
#' @param scan_IDs
#'
#' @return
#' @export
#'
#' @examples
query_UID_scans<- function(scan_IDs){
  result <- query_mongoDB("scans",
                          "UID",
                          scan_IDs)

  return(result)
}
