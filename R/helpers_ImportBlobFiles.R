V <- "290422"
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
    "- extract_h_pixels_from_blob_parameter()",
    "- extract_v_pixel_from_blob_parameter()",
    "- extract_n_pixels_from_blob_parameter()",
    "- extract_image_path_from_blob_parameter()",
    "- extract_image_width_from_blob_parameter()",
    "- extract_image_heigth_from_blob_parameter()",
    "- extract_image_resolution_from_blob_parameter()",
    "- read_binary_image_as_matrix()",
    "- extract_statistics_from_blob_parameter()",
    "- extract_parameter_from_BLOB()",
    "- export_blob_parameter_of_image_filelist()",
    "- create_ScanHistory_extended()",
    "- create_hdr_filepath()",
    "- select_hdr_files()",
    "- query_chipID_channels()",
    "- extract_enabled_positions()",
    "- get_enabled_positions_from_positions_list()",
    "- get_enabled_positions()",
    "- get_positions_field_from_query_result()"
  ))

#' create_hdr_filepath
#'
#' @param chip_path
#' @param scan_ID
#' @param pos_ID
#'
#' @return
#' @export
#'
#' @examples
create_hdr_filepath <- function(chip_path,scan_ID,pos_ID){

  hdr_file_path <- file.path(
    chip_path,
    "scanjobs",
    scan_ID,
    pos_ID,
    "hdr"
  )
}

#' convert_binsize_from_encoding
#'
#' @param encoding
#'
#' @return
#' @export
#'
#' @examples
convert_binsize_from_encoding <- function(encoding){

  Version <- "270422"

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

  pos<-  which(pos_ID %in% c(1:9))
  pos_ID[pos] <- paste0("0", pos_ID[pos])

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

  Version <- "290422"
  #update:
  #- purrr::map_df
  MethodHistory <- create_MethodHistory_of_chipIDs(chip_IDs)

  ScanHistorys <- purrr::map_df(MethodHistory,
                             ~.x%>%
                               dplyr::rename("scan_ID" = "UID")%>%
                               dplyr::rename("cycle_ID" = "CycleUID")%>%
                               tidyr::fill(cycle_ID,  .direction = "up")%>%
                               dplyr::filter(Type == "Chipcytometry-Scan")%>%
                               dplyr::select(scan_ID,cycle_ID,Status,Tag,Excluded,PreparedForDataviz))
  return(ScanHistorys)
}

#' create_ScanHistory_extended
#'
#' @param chip_IDs
#'
#' @return
#' @export
#'
#' @examples
create_ScanHistory_extended <- function(chip_IDs,output_dir,result_ID){

  Version <- "290422"

  tictoc::tic("create extended ScanHistory")

  # create scanHistory (query limslager)
  ScanHistory = create_ScanHistory_of_chipIDs(chip_IDs)

  # add filterset (query limsproc)
  ScanHistory <- ScanHistory%>%
    dplyr::mutate(filterset = query_filterset_of_scanIDs(scan_ID))

  # query result chipID ins scans
  query_chip_scans <- query_mongoDB("scans",
                                    "channelUID",
                                    chip_IDs)

  # select columns
  results_chip_scans <- purrr::map_df(query_chip_scans$result,
                                      ~.x%>%
                                        dplyr::select("chip_ID" = "channelUID",
                                                      "scan_ID" = "UID",
                                                      jobType,
                                                      basePath,
                                                      "jobEndState",
                                                      "success",
                                                      positions,
                                                      `enabled-count`))

  # join ScanHistory
  ScanHistory2 <- dplyr::full_join(ScanHistory,
                                  results_chip_scans, by = "scan_ID")

  # subselect columns position
  ScanHistory3 <- ScanHistory2%>%
    dplyr::mutate(positions = purrr::map(
      positions,
      ~.x%>%
        dplyr::select(
          dplyr::any_of(c("chip_ID",
                          "scan_ID",
                          "pos_ID" = "posid",
                          "jobType",
                          "basePath",
                          "Status",
                          "chipx",
                          "chipy",
                          "bleach-time",
                          "enabled-count",
                          "hdr"
                          #,
                          #"flimages",
                          #"posref",
                          #"focus",
                          #"deltaTL"
                          )))))

  #unnest selected columns in positions
  ScanHistory4 <- ScanHistory3%>%
    tidyr::unnest(positions)

  # extract hdr column in positions
  ScanHistory5 <- ScanHistory4%>%
    dplyr::mutate(hdr_filename = purrr::map(ScanHistory4$hdr,~.x)$filename)%>%
    dplyr::select(-hdr)

 # Image_list <- ScanHistory4%>%
 #   dplyr::select(chip_ID,scan_ID,pos_ID,hdr,flimages,focus,deltaTL,posref)


  # get enabled positions (query in channels)
  enabled_positions <- get_enabled_positions(chip_IDs)

  # join enabled position flag

  ScanHistory6<- dplyr::left_join(ScanHistory5,
                                 enabled_positions,
                                 by=c("chip_ID", "pos_ID"="posid"))

  #add chip_path
  serverpath <- find_server_path()

  chip_paths <- data.frame(
    chip_ID = chip_IDs,
    chip_path = purrr::map_chr(chip_IDs,~find_chip_path(.x)))

  ScanHistory7 <- ScanHistory6%>%
    dplyr::left_join(chip_paths,by="chip_ID")

  # export ScanHistory
  result_filename <- create_result_filepath(output_dir,
                                            "extendedScanHistory",
                                            result_ID,
                                            "csv")
  data.table::fwrite(ScanHistory7,result_filename)

  tictoc::toc()
  return(ScanHistory7)

}

#' export_blob_parameter_of_image_filelist
#'
#' @param image_files
#' @param output_dir
#' @param result_ID
#'
#' @return
#' @export
#'
#' @examples
export_blob_parameter_of_image_filelist<-function(image_files,
                                                  output_dir,
                                                  result_ID){

  V <- "270422"

  parameter_list <- purrr::map2_df(
    image_files$image_path,
    image_files$blob_filename,
    ~extract_parameter_from_BLOB(.x,.y))

  result_filename <- create_result_filepath(output_dir,
                                            "Blob_parameters",
                                            result_ID,
                                            "csv")
  data.table::fwrite(parameter_list,
                     result_filename)

  return(parameter_list)

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

  Version <- "270422"

  encoding <- blob_parameter%>%
    dplyr::filter(node_attributes_id=="encoding")%>%
    dplyr::pull(node_attributes)

  return(encoding)
}

#' extract_h_pixels_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
extract_h_pixels_from_blob_parameter <- function(blob_parameter){

  Version <- "270422"

  h_pixel <- blob_parameter%>%
    dplyr::filter(node_name == "size")%>%
    dplyr::filter(node_attributes_id=="width")%>%
    dplyr::pull(node_attributes)%>%
    as.numeric()

  return(h_pixel)
}

#' extract_image_path_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
extract_image_path_from_blob_parameter<- function(blob_parameter){

  Version <- "270422"

  path <- file.path(attr(blob_parameter, "image_path"),
                    attr(blob_parameter, "blob_filename"))

  return(path)
}

#' extract_image_heigth_from_blob_parameter
#'
#' @param blob_paramter
#'
#' @return
#' @export
#'
#' @examples
extract_image_heigth_from_blob_parameter <- function(blob_paramter){

  Version <- "270422"

  heigth <-   blob_parameter%>%
    dplyr::filter(node_name == "extent")%>%
    dplyr::filter(node_attributes_id=="height")%>%
    dplyr::pull(node_attributes)%>%
    as.numeric()

  return(heigth)
}

#' extract_image_resolution_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
extract_image_resolution_from_blob_parameter <- function(blob_parameter){

  Version <- "270422"

  v_pixel <- extract_v_pixel_from_blob_parameter(blob_parameter)
  width <- extract_image_width_from_blob_parameter(blob_parameter)
  v_res <- width/v_pixel

  h_pixel <- extract_h_pixels_from_blob_parameter(blob_parameter)
  heigth <- extract_image_heigth_from_blob_parameter(blob_parameter)
  h_res <- heigth/h_pixel

  resolution_unit <- "[um per pixel edge]"

  image_resolution <- c(h_res,v_res)
  attr(image_resolution,"resolution_unit") <- resolution_unit

  return(image_resolution)
}

#' extract_image_width_from_blob_parameter
#'
#' @param blob_paramter
#'
#' @return
#' @export
#'
#' @examples
extract_image_width_from_blob_parameter <- function(blob_paramter){

  Version <- "270422"

  width <-   blob_parameter%>%
    dplyr::filter(node_name == "extent")%>%
    dplyr::filter(node_attributes_id=="width")%>%
    dplyr::pull(node_attributes)%>%
    as.numeric()

  return(width)
}

#' extract_n_pixels_from_blob_parameter
#'
#' @param blob_paramter
#'
#' @return
#' @export
#'
#' @examples
extract_n_pixels_from_blob_parameter <- function(blob_paramter){

  Version <- "270422"

  h_pixel <- extract_h_pixels_from_blob_parameter(blob_parameter)
  height <- extract_v_pixels_from_blob_parameter(blob_parameter)
  n_pixels <- h_pixel * height

  return(n_pixels)
}

#' extract_v_pixels_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
extract_v_pixels_from_blob_parameter <- function(blob_parameter){

  Version <- "270422"

  v_pixel <- blob_parameter%>%
    dplyr::filter(node_name == "size")%>%
    dplyr::filter(node_attributes_id=="height")%>%
    dplyr::pull(node_attributes)%>%
    as.numeric()

  return(v_pixel)
}

#' extract_parameter_from_BLOB
#'
#' @param image_path
#' @param blob_filename
#'
#' @return
#' @export
#'
#' @examples
extract_parameter_from_BLOB <- function(image_path,
                                        blob_filename){

  Version <- "270422"

  blob_parameter <- read_XML_BLOB_parameter(image_path,
                                            blob_filename)
  df <- data.frame(
    image_path = image_path,
    blob_filename = blob_filename,
    h_pixel = extract_h_pixels_from_blob_parameter(blob_parameter),
    v_pixel = extract_v_pixels_from_blob_parameter(blob_parameter),
    n_pixel = extract_n_pixels_from_blob_parameter(blob_parameter),
    image_width = extract_image_width_from_blob_parameter(blob_parameter),
    image_heigth = extract_image_heigth_from_blob_parameter(blob_parameter))%>%
    dplyr::mutate(
      h_res = image_width/h_pixel,
      v_res = image_heigth/v_pixel)

  df <- cbind(df,
              extract_statistics_from_blob_parameter((blob_parameter)))

  return(df)
}

#' extract_statistics_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#'
#' @examples
extract_statistics_from_blob_parameter <- function(blob_parameter){

  Version <- "270422"

  statistics <-   blob_parameter%>%
    dplyr::filter(node_name == "statistics")%>%
    dplyr::mutate(node_attributes = as.numeric(node_attributes))%>%
    tidyr::spread(node_attributes_id,node_attributes)%>%
    dplyr::select(-node_name, -node_path)

  return(statistics)
}

#' extract_enabled_positions
#'
#' @param single_pos_entity
#'
#' @return
#' @export
#'
#' @examples
extract_enabled_positions <- function(single_pos_entity){
  df <- data.frame(
    pos_ID = single_pos_entity['posid'],
    enabled = single_pos_entity['enabled']
  )#%>%
  #  dplyr::filter(enabled == 1)
  return(df)
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
#' get_enabled_positions
#'
#' @param chip_ID
#'
#' @return
#' @export
#'
#' @examples
get_enabled_positions <- function(chip_ID){

  query_result<-query_chipID_channels(chip_IDs)
  positions_list <- get_positions_field_from_query_result(query_result)
  enabled_positions <- get_enabled_positions_from_positions_list(positions_list)

  return(enabled_positions)
}

#' get_enabled_positions_from_positions_list
#'
#' @param positions_list
#'
#' @return
#' @export
#'
#' @examples
get_enabled_positions_from_positions_list <- function(positions_list){

  enabled_positions <-  tibble::tibble(
    chip_ID = positions_list$chip_ID,
    enabled_positions = purrr::map(positions_list$positions,
                                   ~.x%>%extract_enabled_positions()))%>%
    tidyr::unnest(cols=c("enabled_positions"))
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

#' get_positions_field_from_query_result
#'
#' @param query_result
#'
#' @return
#' @export
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

#' read_binary_image_as_matrix
#'
#' @param image_path
#' @param blob_filename
#'
#' @return
#' @export
#'
#' @examples
read_binary_image_as_matrix <- function(image_path,
                                        blob_filename){

  Version <- "270422"

  blob_parameter <-read_XML_BLOB_parameter(image_path,
                                           blob_filename)

  data <- read_image_binary_file(blob_parameter)

  data_mat <- matrix(data,
                     nrow = attr(data, "v_pixel"),
                     ncol = attr(data,"h_pixel"))

  attr(data_mat, "image_resolution") <- attr(data,"image_resolution")

  return(data_mat)
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

  Version <- "270422"

  encoding<-blob_parameter%>%
    extract_encoding_from_blob_parameter()

  bin_size<-encoding%>%
    convert_binsize_from_encoding()

  n_pixels<- blob_parameter%>%
    extract_n_pixels_from_blob_parameter()

  path <- blob_parameter%>%
    extract_image_path_from_blob_parameter()

  data<-readBin(path,
                integer(),
                n=n_pixels,
                size=bin_size)

  attr(data,"h_pixel") <- extract_h_pixels_from_blob_parameter(blob_parameter)
  attr(data,"v_pixel") <- extract_v_pixels_from_blob_parameter(blob_parameter)
  attr(data,"image_resolution") <- extract_image_resolution_from_blob_parameter(blob_parameter)

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

#' select_hdr_files
#'
#' @param results_files
#'
#' @return
#' @export
#'
#' @examples
select_hdr_files <- function(results_files){
  hdr_files <- result_files%>%
    dplyr::filter(!is.na(hdr_filename))%>%
    #dplyr::filter(jobType == "FL")%>%
    #dplyr::filter(! Tag %in% c("BG", "*"))%>%
    dplyr::mutate(hdr_filepath = create_hdr_filepath(chip_path,scan_ID,pos_ID))#%>%
  #dplyr::group_by(chip_ID,pos_ID)%>%
  #tidyr::nest()

  return(hdr_files)
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
select_valid_image_files <- function(result_files, type=NULL){

  #Version <- "250422"
  Version <- "290422"
  # - added NULL as default type
  # - include filtering of enabled flag

  #_______________
  # 0) check input----
  type <- match.arg(type, choices =c("none","blob","blob32","png"))

  #_________________________
  # 1) remove excluded scans----
  result_files <- result_files%>%
    dplyr::filter(Excluded %in% c("FALSE"))%>%
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

  #____________________________
  # 3) filter enabled positions
  if(1 %in% result_files$enabled){
    result_files <- result_files%>%
      dplyr::filter(enabled == 1)
  }


  #_________________
  # 4) return result----
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

#' Title
#'
#' @param chip_IDs
#'
#' @return
#' @export
#'
#' @examples
query_chipID_channels <- function(chip_IDs){

  query_result <- query_mongoDB("channels","UID",chip_IDs)
  return(query_result)

}
