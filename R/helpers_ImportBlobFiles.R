V <- "250522"
helpers <- "ImportBlobFiles"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_hdr_filepath()",
    "- create_pos_foldername()",
    "- create_Pos_image_filepath()",
    "- convert_binsize_from_encoding()",
    "- extract_enabled_positions()",
    "- extract_encoding_from_blob_parameter()",
    "- extract_h_pixels_from_blob_parameter()",
    "- extract_image_path_from_blob_parameter()",
    "- extract_image_heigth_from_blob_parameter()",
    "- extract_image_resolution_from_blob_parameter()",
    "- extract_image_width_from_blob_parameter()",
    "- extract_n_pixels_from_blob_parameter()",
    "- extract_v_pixel_from_blob_parameter()",
    "- extract_parameter_from_BLOB()",
    "- extract_statistics_from_blob_parameter()",
    "- get_enabled_positions_from_positions_list()",
    "- list_posFolders_in_ScanBasePath()",
    "- read_binary_image_as_matrix()",
    "- read_binary_image_as_vector()",
    "- read_image_binary_file()",
    "- read_XML_BLOB_parameter()",
    "- select_hdr_files()",
    "- select_valid_image_files()"
  ))

#' create_hdr_filepath
#'
#' @param chip_path
#' @param scan_ID
#' @param pos_ID
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
create_hdr_filepath <- function(chip_path,scan_ID,pos_ID){

  pos<-  which(pos_ID %in% c(1:9))
  pos_ID[pos] <- paste0("0", pos_ID[pos])

  hdr_file_path <- file.path(
    chip_path,
    "scanjobs",
    scan_ID,
    paste0("pos",pos_ID),
    "hdr"
  )
}


#' create_pos_foldername
#'
#' @param imageServer_path
#' @param basePath
#' @param pos_ID
#'
#' @return
#' @export
#' @keywords internal
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
#' @keywords internal
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

#' convert_binsize_from_encoding
#'
#' @param encoding
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
convert_binsize_from_encoding <- function(encoding){

  Version <- "270422"

  bin_size <- dplyr::case_when(encoding == "32bit little-endian" ~ 4,
                               encoding == "16bit little-endian" ~ 2)

  return(bin_size)
}

#' extract_enabled_positions
#'
#' @param single_pos_entity
#'
#' @return
#' @export
#' @keywords internal
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

#' extract_encoding_from_blob_parameter
#'
#' @param blob_parameter
#'
#' @return
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
#'
#' @examples
extract_image_heigth_from_blob_parameter <- function(blob_parameter){

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
#' @keywords internal
#'
#' @examples
extract_image_resolution_from_blob_parameter <- function(blob_parameter){

  Version <- "270422"

  v_pixel <- extract_v_pixels_from_blob_parameter(blob_parameter)
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
#' @keywords internal
#'
#' @examples
extract_image_width_from_blob_parameter <- function(blob_parameter){

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
#' @keywords internal
#'
#' @examples
extract_n_pixels_from_blob_parameter <- function(blob_parameter){

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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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






#' get_enabled_positions_from_positions_list
#'
#' @param positions_list
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
get_enabled_positions_from_positions_list <- function(positions_list){

  enabled_positions <-  tibble::tibble(
    chip_ID = positions_list$chip_ID,
    enabled_positions = purrr::map(positions_list$positions,
                                   ~.x%>%extract_enabled_positions()))%>%
    tidyr::unnest(cols=c("enabled_positions"))
}



#' list_posFolders_in_ScanBasePath
#'
#' @param ScanBasePath
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
list_posFolders_in_ScanBasePath <- function(ScanBasePath){
  positions<-list.files(path=ScanBasePath)
  positions=positions[stringr::str_detect(positions,"pos")]
  return(positions)
}





#' read_binary_image_as_matrix
#'
#' @param image_path
#' @param blob_filename
#'
#' @return
#' @export
#' @keywords internal
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

#' read_binary_image_as_vector
#'
#' @param image_path
#' @param blob_filename
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
read_binary_image_as_vector<- function(image_path,
                            blob_filename){
  Version <- "030522"

  blob_parameter <-read_XML_BLOB_parameter(image_path,
                                           blob_filename)

  data <- read_image_binary_file(blob_parameter)

  return(data)


}
#' read_image_binary_file
#'
#' @param blob_parameter
#'
#' @return
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
#'
#' @examples
select_hdr_files <- function(result_files){

  hdr_files <- result_files%>%
    dplyr::filter(!is.na(hdr_filename))%>%
    #dplyr::filter(jobType == "FL")%>%
    dplyr::filter(! Tag %in% c("BG", "*"))%>%
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
#' @keywords internal
#'
#' @examples
select_valid_image_files <- function(result_files, type=NULL){

  #Version <- "250422"
  Version <- "290422"
  # - added NULL as default type
  # - include filtering of enabled flag

  #___________
  #check input----
  type <- match.arg(type, choices =c("none","blob","blob32","png"))

  #_____________________
  #remove excluded scans----
  result_files <- result_files%>%
    dplyr::filter(Excluded %in% c("FALSE"))%>%
    dplyr::filter(Status == "Finished")

  #___________________________
  #select image type to return----
  # __blob----
  if(type == "blob"){
    result_files <- result_files%>%
      dplyr::filter(filetype == "blob")
  }
  # __blob32----
  if(type == "blob32"){
    result_files <- result_files%>%
      dplyr::filter(filetype == "blob32")
  }
  # __png----
  if(type == "png"){
    result_files <- result_files%>%
      dplyr::filter(filetype == "png")
  }

  #________________________
  #filter enabled positions
  if(1 %in% result_files$enabled){
    result_files <- result_files%>%
      dplyr::filter(enabled == 1)
  }

  #_____________
  #return result----
  return(result_files)
}

