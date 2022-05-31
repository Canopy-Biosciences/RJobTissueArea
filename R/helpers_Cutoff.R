V <- "300522"
helpers <- "MarkerCutoff"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "-"
  ))


#' searches Chip_IDs in limslager
#'
#' @param chip_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' chip_IDs <- c("M911981", "M911971")
search_chip_limslager <- function(chip_IDs){

  chip_limslager <- query_mongoDB("limslager",
                                  "UID",
                                  chip_IDs)

  return(chip_limslager)

}
#' extracts ObjRefs Type and UIDs
#'
#' @param chip_limslager
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
extract_ObjRef_chip_limslager <- function(chip_limslager){

  ObjRef <- purrr::map(chip_limslager$result,
                          ~data.frame(
                            chip_ID = .x$UID,
                            DataViz = .x$EDL%>%
                              xml2::read_xml()%>%
                              xml2::xml_find_all("*//ObjRef")%>%
                              purrr::map(xml2::xml_attrs)%>%
                              purrr::map_df(~as.list(.))))%>%
    dplyr::bind_rows()

  return(ObjRef)
}
#' filters CytometryDatavizParametersSample UIDs in ObjRef
#'
#' @param ObjRef
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
filter_ObjRef_CytometryDatavizParametersSample <- function(ObjRef){

  DataVizParametersSample <- ObjRef%>%
    dplyr::filter(DataViz.Type == "CytometryDatavizParametersSample")%>%
    dplyr::select(chip_ID,"DataViz_ID"="DataViz.UID")

  return(DataVizParametersSample)
}
#' filters Gate UIDs in ObjRef
#'
#' @param ObjRef
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
filter_ObjRef_Gate <- function(ObjRef){

  Gates <- ObjRef%>%
    dplyr::filter(DataViz.Type == "Gate")%>%
    dplyr::select(chip_ID,"gate_ID"="DataViz.UID")

  return(Gates)
}

#' searches gate_IDs in limslager
#'
#' @param gate_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
search_gate_limslager <- function(gate_IDs=Gates$gate_ID){

  gate_limslager <- query_mongoDB("limslager",
                                  "UID",
                                  gate_IDs)

  return(gate_limslager)
}

#' extracts name of gates
#'
#' @param gate_limslager
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
extract_gate_name <- function(gate_limslager){

  gateName <- purrr::map_df(gate_limslager$result,
                            ~data.frame(
                              gate_ID = .x$UID,
                              gate_name =.x$EDLName)
  )
  return(gateName)
}



#' extracts gates metadata
#'
#' This functions takes the return object of a mongo-collection limslager query for gate_IDs. The MetaData including gate_ID and the corresponding gateName, segment_ID, ParentGate_ID, x_marker_UID and y_marker_UID were combined and returned as dataframe.
#'
#' @param gate_limslager
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
extract_gate_metadata<-function(gate_limslager){

  xml_nodes <- gate_MetaData <- NULL

  xml_nodes <- create_query_result_nodes(gate_limslager)

  gate_MetaData <- purrr::map2_df(gate_limslager$result,
                                  xml_nodes,
                                 ~data.frame(
                                   gate_ID = .x$UID,
                                   gate_name =.x$EDLName,
                                   segment_ID = .y%>%extract_xml_value("//*/SpecificParameter[@Name='Parentsegmentation']"),
                                   ParentGate_ID = .y%>%extract_xml_value("//*/SpecificParameter[@Name='Parentgate']"),
                                   x_marker_UID = .y%>%extract_xml_value("//*/SpecificParameter[@Name='x_marker_UID']"),
                                   y_marker_UID = .y%>%extract_xml_value("//*/SpecificParameter[@Name='y_marker_UID']")))

  return(gate_MetaData)
}

query_mongoDB("limsproc","LIMSID",gate_MetaData$gate_ID)->test

#' searches CytometryDatavizParametersSample UID in limslager
#'
#' @param DataViz_ID
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' DataViz_ID <- c("D972192","D972187")
search_CytometryDatavizParametersSample_limslager <- function(DataViz_ID){
  DataViz_limslager<- query_mongoDB("limslager",
                                    "UID",
                                    DataViz_ID)
  return(DataViz_limslager)
}


#' extracts CytometryDatavizParametersMarker UID
#'
#' @param DataViz_ID
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
extract_VizMarker_ID <- function(DataViz_limslager){

  DataVizMarker <- purrr::map_df(DataViz_limslager$result,
                       ~data.frame(
                         DataViz_ID = .x$UID,
                         VizMarker_ID=.x$EDL%>%
                           xml2::read_xml()%>%
                           xml2::xml_find_all("*//ObjRef[@Type = 'CytometryDatavizParametersMarker']")%>%
                           xml2::xml_attr("UID")))

  return(DataVizMarker)
}

#' searches CytometryDatavizParametersMarker UID in limslager
#'
#' @param Viz_Marker_ID
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' VizMarker_ID <- c("D972732", "D972733", "D972734", "D972735", "D972736", "D972737", "D972787", "D972794",
#' "D972795", "D972796", "D972842", "D972844", "D972638", "D972639", "D972640", "D972683", "D972684",
#' "D972685", "D972790", "D972791", "D972793", "D972820", "D972843")
search_CytometryDatavizParametersMarker_limslager <- function(VizMarker_ID){
  VizMarker_limslager <- query_mongoDB("limslager","UID",VizMarker_ID)
  return(VizMarker_limslager)
}

#' extracts VizMarkers Gate High and Low Values
#'
#' @param VizMarker_limslager
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
extract_Marker_Cutoff <- function(VizMarker_limslager){

  xml_nodes <- create_query_result_nodes(VizMarker_limslager)

  MarkerCutoff <- purrr::map2_df(VizMarker_limslager$result,
                                 xml_nodes,
                             ~data.frame(
                               VizMarker_ID=.x$UID,
                               GateHigh = .y%>%extract_xml_value("*//SpecificParameter[@Name = 'GateHigh']"),
                               GateLow = .y%>%extract_xml_value("*//SpecificParameter[@Name = 'GateLow']")
                             ))
  return(MarkerCutoff)
}



#' creates xml-nodes of EDL query result
#'
#' @param EDL
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
create_xml_nodes <- function(EDL){

  xml_nodes <- purrr::map(EDL,
                          ~.x%>%
                            xml2::read_xml())
  return(xml_nodes)


}

#' returns values of all nodes at a given xpath command
#'
#' @param nodes
#' @param find_string
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' find_string <- "//*/SpecificParameter[@Name='Parentsegmentation']"
#' find_string <- "//*/SpecificParameter[@Name='Parentgate']"
#' find_string <- "//*/SpecificParameter[@Name='x_marker_UID']"
#' find_string <- "//*/SpecificParameter[@Name='y_marker_UID']"
#' xml_node <- xml_nodes[[1]]
#'
extract_xml_value<- function(xml_node,find_string){
  xml_value <- NULL
  xml_value<- xml_node%>%
    xml2::xml_find_all(find_string)%>%
    xml2::xml_attr("Value")
  if(rlang::is_empty(xml_value)){xml_value = NA}
  if("" %in% xml_value){xml_value[which(xml_value == "")] <- NA}


  return(xml_value)
}

#' creates xml-nodes of query result EDL
#'
#' @param query_result
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
create_query_result_nodes <- function(query_result){

  xml_nodes<-query_result%>%
    get_EDL_from_query_result()%>%
    create_xml_nodes()

  return(xml_nodes)
}
