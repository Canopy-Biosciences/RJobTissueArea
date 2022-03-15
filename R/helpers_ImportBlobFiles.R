V <- "150322"
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
    "- query_UID_scans()"
  ))

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
