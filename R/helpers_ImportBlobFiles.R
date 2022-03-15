V <- "130322"
helpers <- "ImportBlobFiles"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_Pos_image_filepath()",
    "- find_scan_basepath()",
    "- get_df_from_query_result()",
    "- list_BlobFileName_in_filepath()",
    "- list_posFolders_in_ScanBasePath()",
    "- query_UID_scans()"
  ))


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
