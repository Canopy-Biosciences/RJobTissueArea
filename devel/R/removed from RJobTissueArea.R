V <- "300622"
helpers <- "RemovedFromRJobTissueArea"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- collect_segments_metadata()",
    "- extract_gate_metadata()",
    "- find_all_attributes_in_EDL()",
    "- find_RService_XML_on_imageserver()",
    "- find_scan_basepath()",
    "- get_df_from_query_result()",
    "- get_identityID_from_EDL()",
    "- query_segment_ID()",
    "- query_UID_scans()",
    "- create_Pos_image_filepath()",
    "- export_blob_parameter_of_image_filelist()",
    "- extract_parameter_from_BLOB()",
    "- extract_statistics_from_blob_parameter()",
    "- list_posFolders_in_ScanBasePath()",
    "- create_ScanHistory_from_MethodHistory()",
    "- create_pos_df_from_MethodHistory()",
    "- determine_scan_position()",
    "- extract_cycle_sequence_from_MethodHistory()",
    "- extract_OK_pos_from_MethodHistory()",
    "- extract_Ref_pos_from_MethodHistory()",
    "- extract_XYvalues_pos_from_MethodHistory()",
    "- get_gate_ID_of_AllGate()",
    "- get_segment_ID_of_chipID()",
    "- get_pos_colnames_in_MethodHistory()",
    "- get_sampleType_from_MethodHistory()",
    "- read_ScanHistory()",
    "- return_segment_metadata()",
    "- select_segment_ID()",
    "- add_dependencies_to_DESCRIPTION()",
    "- download_pkg_sources()",
    "- get_package_dependencies()",
    "- handle_trychache_error()",
    "- write_lines_task()",
    "- ()"
  ))



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
#' @keywords internal
#'
#' @examples
#'
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

#' Title
#'
#' @param chip_IDs
#'
#' @return
#' @keywords internal
#'
#' @examples
extract_gate_metadata <- function(chip_IDs){

  #V120222 - initial version
  #_____________________________
  V <- "230222"
  #- extraction of AllObjRefs out of result$result$EDL
  #- bugfix
  v<- "080322"
  #- added purrr::, tidyr::
  V<- 300522
  #- adding Parent-gate_UID

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
                                                        xml2::xml_attr("Value")),
                  ParentGate = purrr::map_chr(result2$result,
                                              ~.x$EDL%>%
                                                xml2::read_xml()%>%
                                                xml2::xml_find_all("//*/SpecificParameter[@Name='Parentgate']")%>%
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

#____________________________
#find_all_attributes_in_EDL()
#____________________________

## chunk function_find_all_attributes_in_EDL{

#' @title find_all_attributes_in_EDL()
#'
#' @description Reads a XML string, extracts all nodes, attributes and paths, reorganize the data and returns 3 different dataframes
#'
#' @details Die Funktion erzeugt zunächst aus dem EDL String ein XML formatiertes Objekt.
#' Von diesem XML Objekt werden alle Knoten ermittelt und für jeden Knoten ein XML-Nodeset, einschließlich der Dokumentwurzel, erstellt und als Spalte xml_node in den dataframe df eingefügt.
#' Ausgehend von den Nodesets wird für jeden Knoten der Name und der Pfad sowie alle Attribute ermittelt bzw. extrahiert und als Spalten node_name, node_path, node_attribute in df eingefügt.
#' Dabei ist im dataframe df die Spalte node_attributes eine genestete Spalte, die je nach Knoten eine unterschiedliche Anzahl an Attributen enthalten kann.
#' Die genestete Spalte node_attributes wird zwei reguläre Spalten node_attributes,node_attributes_id transformiert und mit den Spalten node_name,node_path,xml_node zum df-longer zusammengefasst.
#' Der df_wider enthält die gleichen Daten wie df_longer, mit dem Unterschied das für jeden Attributnamen eine Spalte eingefügt wurde, und die Attributwerte somit horizontal verteilt werden,  sodass die Zeilenanzahl der vom df entspricht.
#'
#' @param EDL :character containing a XML string
#'
#' @return a list of 3 dataframes df, df_longer, df_wider
#' @export
#' @keywords internal
#' @family database related
#'
#' @examples
#' if(FALSE){
#'
#' result_list <- find_all_attributes_in_EDL(EDL)
#' }
#'
find_all_attributes_in_EDL<-function(EDL){
  # version: 061021

  file <- try(EDL%>%
                xml2::read_xml(),
              silent=TRUE)

  if(inherits(file,'try-error')==FALSE){

    # create object containing all nodes in the EDL, including the root
    all_nodes <-file %>%
      xml2::xml_find_all(".//*")
    all_nodes<-c(list(file),purrr::map(all_nodes,~.x))
    print(paste0("EDL contains ",length(all_nodes)," nodes - including the EDL itself"))

    # create df containing a column with all nodes of the EDL
    df <-data.frame(xml_node=vector(length=length(all_nodes)))%>%
      dplyr::mutate(xml_node=purrr::map(all_nodes,~.x))

    #add name of node-object
    names(df$xml_node)<-purrr::map_chr(df$xml_node,~xml2::xml_name(.x))

    # extract node attributes and add them to df
    df <- df%>%
      dplyr::mutate(node_name=purrr::map_chr(xml_node,~.x%>%xml2::xml_name()),
                    node_attributes=purrr::map(xml_node,~.x%>%xml2::xml_attrs()),
                    node_path=purrr::map_chr(xml_node,~.x%>%xml2::xml_path()))

    # remove object numeration from path
    #df<-df%>%
    #  mutate(node_path=node_path %>% stringr::str_remove_all("\\[[0-9]+\\]"))

    # unnest the df, so that all attribute Values and Names are in a column
    df_longer <- df %>%
      tidyr::unnest_longer(node_attributes)%>%
      dplyr::select(node_name,node_attributes,node_attributes_id,node_path,xml_node)

    # unnest the df with attribute_id as columns
    df_wider<-df%>%
      tidyr::unnest_wider(node_attributes)

    return(attribute_dfs=list(df=df,df_longer=df_longer,df_wider=df_wider))

  }else(return("error_cant read input as XML"))
}
##}

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
#' find_scan_basepath
#'
#' @param scan_IDs
#'
#' @return
#' @export
#' @keywords internal
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
#' @keywords internal
#'
#' @examples
get_df_from_query_result<- function(query_result){

  result <- purrr::map_df(query_result$result, ~.x)

  return(result)

}

#' Title
#'
#' @param EDL
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
get_identityID_from_EDL<-function(EDL){
  chipUID <-EDL%>%
    xml2::read_xml()%>%
    xml2::xml_find_first("Identity")%>%
    xml2::xml_attr("UID")
  return(chipUID)
}

#' Title
#'
#' @param segment_IDs
#'
#' @return data.frame
#' @export
#' @keywords internal
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

#' query_UID_scans
#'
#' @param scan_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
query_UID_scans<- function(scan_IDs){
  result <- query_mongoDB("scans",
                          "UID",
                          scan_IDs)

  return(result)
}





## **collect_segments_metadata()**
# takes a chip_path (defined by server volume and chip_ID)
# creates the segment_path
# lists all segments folders in segment_path
# determines when each folder was last modified (on disk)
# calls query_segment_status(), which extracts the mongoDB Metho-Status and its last-change value
## Title
#
# @param chip_path
#
# @return result
# @export
# @keywords internal
#
# @examples
# chip_path <- c("\\\\intern.chipcytometry.com\\imagedata\\leipzig_volume0\\M986054")
# collect_segments_metadata(chip_path)
##``
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




#```
#
#```{r}
#chip_ll <- query_UID_limslager(chip_IDs = IDs$chip_ID)
#```
#
#### **get_EDL_from_query_result()**
#
#```{r}


#```
#
#```{r}
#
#EDL <- query_UID_limslager(
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

#' export_blob_parameter_of_image_filelist
#'
#' @param image_files
#' @param output_dir
#' @param result_ID
#'
#' @return
#' @export
#' @keywords internal
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


#' create_ScanHistory_from_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
#' @keywords internal
#'
#' @examples
create_ScanHistory_from_MethodHistory<-function(MethodHistory){

  ScanHistorys <- purrr::map(MethodHistory,
                             ~.x%>%
                               dplyr::rename("scan_ID" = "UID")%>%
                               dplyr::rename("cycle_ID" = "CycleUID")%>%
                               tidyr::fill(cycle_ID,  .direction = "up")%>%
                               dplyr::filter(Type == "Chipcytometry-Scan")%>%
                               dplyr::select(scan_ID,cycle_ID,Status,Tag,Excluded,PreparedForDataviz))
  return(ScanHistorys)
}

#' create_pos_df_from_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
#' @keywords internal
#'
#' @examples
create_pos_df_from_MethodHistory <- function(MethodHistory){
  columns <- purrr::map(MethodHistory,
                        ~.x%>%get_pos_colnames_in_MethodHistory)
  df <- purrr::map2(MethodHistory,
                    columns,
                    ~.x[,.y])
  df <- purrr::map(df,
                   ~.x%>%
                     tidyr::gather("column",
                                   "value",
                                   -UID,
                                   -CycleUID,
                                   -Excluded,
                                   -PreparedForDataviz))
  df <- purrr::map(df,
                   ~if(dim(.x)[2] == 4){
                     .x$column <- rep(NA,times=dim(.x)[1])
                     .x$value <- rep(NA,times=dim(.x)[1])
                     return(.x)
                   }else{return(.x)})
  return(df)
}

#' Title
#'
#' @param MethodHistory
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
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


#' extract_cycle_sequence_from_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
#' @keywords internal
#'
#' @examples
extract_cycle_sequence_from_MethodHistory <- function(MethodHistory){

  cycle_sequence <- purrr::map(MethodHistory,
                               ~.x%>%
                                 dplyr::filter(Type == "Chipcytometry-Cycle")%>%
                                 dplyr::rename("cycle_ID" = "UID")%>%
                                 dplyr::mutate(cycle_pos = 1:dplyr::n())%>%
                                 dplyr::select(cycle_pos,cycle_ID,Status,PreparedForDataviz))
  return(cycle_sequence)
}

#' extract_OK_pos_from_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
#' @keywords internal
#'
#' @examples
extract_OK_pos_from_MethodHistory <- function(MethodHistory){

  df <- create_pos_df_from_MethodHistory(MethodHistory)

  OK_col <- purrr::map(df,
                       ~which(stringr::str_detect(.x$column,"OK")==TRUE))

  OK_scan <- purrr::map2(df,
                         OK_col,
                         ~.x[.y,])

  pos_df_OK <- purrr::map(OK_scan,
                          ~.x%>%
                            dplyr::filter(value== "True")%>%
                            dplyr::mutate(pos = column)%>%
                            dplyr::mutate(pos = stringr::str_remove(pos,"pos"))%>%
                            dplyr::mutate(pos = stringr::str_remove(pos,"OK"))%>%
                            dplyr::group_by(UID)%>%
                            dplyr::summarize(pos_OK = paste0(pos,collapse=", ")))

  return(pos_df_OK)
}

#' extract_Ref_pos_from_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
#' @keywords internal
#'
#' @examples
extract_Ref_pos_from_MethodHistory <- function(MethodHistory){

  result <- create_pos_df_from_MethodHistory(MethodHistory)

  Ref_col <- purrr::map(result,
                        ~which(stringr::str_detect(.x$column,"Ref")==TRUE))

  Ref_scan <- purrr::map2(result,
                          Ref_col,
                          ~.x[.y,])

  pos_df_Ref <- purrr::map(Ref_scan,
                           ~.x%>%
                             dplyr::filter(value== "True")%>%
                             dplyr::mutate(pos = column)%>%
                             dplyr::mutate(pos = stringr::str_remove(pos,"pos"))%>%
                             dplyr::mutate(pos = stringr::str_remove(pos,"Ref"))%>%
                             dplyr::group_by(UID)%>%
                             dplyr::summarize(pos_Ref = paste0(pos,collapse=", "))
  )

  return(pos_df_Ref)
}

#' extract_XYvalues_pos_from_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
#' @keywords internal
#'
#' @examples
extract_XYvalues_pos_from_MethodHistory <- function(MethodHistory){

  df <- create_pos_df_from_MethodHistory(MethodHistory)

  X_col <- purrr::map(df,
                      ~which(stringr::str_detect(.x$column,"X")==TRUE))
  X_scan <- purrr::map2(df,
                        X_col,
                        ~.x[.y,])
  pos_df_X <- purrr::map(X_scan,
                         ~.x%>%
                           dplyr::filter(!is.na(value))%>%
                           dplyr::mutate(pos = column)%>%
                           dplyr::mutate(pos = stringr::str_remove(pos,"pos"))%>%
                           dplyr::mutate(pos = stringr::str_remove(pos,"X"))%>%
                           dplyr::group_by(UID)%>%
                           dplyr::summarize(pos = paste0(pos,collapse=", "),
                                            value_X = paste0(value,collapse=", ")))

  Y_col <- purrr::map(df,
                      ~which(stringr::str_detect(.x$column,"Y")==TRUE))
  Y_scan <- purrr::map2(df,
                        Y_col,
                        ~.x[.y,])
  pos_df_Y <- purrr::map(Y_scan,
                         ~.x%>%
                           dplyr::filter(!is.na(value))%>%
                           dplyr::mutate(pos = column)%>%
                           dplyr::mutate(pos = stringr::str_remove(pos,"pos"))%>%
                           dplyr::mutate(pos = stringr::str_remove(pos,"Y"))%>%
                           dplyr::group_by(UID)%>%
                           dplyr::summarize(pos = paste0(pos,collapse=", "),
                                            value_Y = paste0(value,collapse=", ")))

  pos_df_XY <- purrr::map2(pos_df_X,
                           pos_df_Y,
                           ~dplyr::full_join(
                             .x%>%
                               tidyr::separate_rows(pos,value_X,sep =", "),
                             .y%>%
                               tidyr::separate_rows(pos,value_Y,sep =", "),
                             by = c("UID", "pos")
                           ))

  return(pos_df_XY)


}

#' Title
#'
#' @param chip_ID
#' @param segment_ID
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
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



#' Title
#'
#' @param chip_ID
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
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



#' get_pos_colnames_in_MethodHistory
#'
#' @param MH
#'
#' @return
#' @keywords internal
#'
#' @examples
get_pos_colnames_in_MethodHistory <- function(MH){
  cols <- colnames(MH)
  col_pos <- which(stringr::str_detect(cols,"pos")==TRUE)
  col_scanID <- which(cols %in% c("UID","CycleUID","Taq","Excluded","PreparedForDataviz"))
  return(c(col_scanID,col_pos))
}


#' Title
#'
#' @param MethodHistory
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
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

#' read_ScanHistory
#'
#' @param group_ID
#' @param output_dir
#'
#' @return
#' @export
#' @keywords internal
#'
#'
#' @examples
read_ScanHistory <- function(group_ID,
                             output_dir){
  filename <- create_result_filepath(output_dir,
                                     "extendedScanHistory",
                                     group_ID,
                                     "csv")

  ScanHistory <- data.table::fread(filename)

  return(ScanHistory)
}

### **return_segments_metadata()**

#- calls find_chip_path() and collect_segment_metadata() for several chipIDs


#' Title
#'
#' @param chip_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
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

#' Title
#'
#' @param segment_IDs
#'
#' @return
#' @keywords internal
#'
#' @examples
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

#' find and add all package dependencies to DESCRIPTION file
#'
#'
#'
#' @param exact_version character vector of package names where exact version is obligatory
#' @param ignore_folder character vector of folder names in pkg directory which are build-ignored
#' @param ignore_package character vector of package names to remove from final list
#'
#' @return the DESCRIPTION file is overwritten
#' @export
#' @keywords internal
#'
#' @examples
#' if(FALSE){
#' add_dependencies_to_DESCRIPTION(
#' exact_version = c("mongolite","locfit","EBImage"),
#' ignore_folder <- c("devel"),
#' ignore_package <- "RJobTissueArea"
#'  )
#'  }
add_dependencies_to_DESCRIPTION <- function(exact_version = "mongolite",
                                            ignore_folder = "devel",
                                            ignore_package = "RJobTissueArea"){

  deps_import <- get_package_dependencies(exact_version ,ignore_folder, ignore_package)

  purrr::walk(deps_import$Package,
              ~usethis::use_package(.x,"imports"))
  #add exact version to DESCRIPTION
  purrr::walk2(exact_version,
               rep(TRUE,length(exact_version)),
               ~usethis::use_package(.x,"imports",.y))

}



#' Title
#'
#' @param Package
#' @param Version
#' @param lib_path
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' if(FALSE){
#' dep_pkg <- get_package_dependencies()
#' Package <- dep_pkg$Package
#' Version <- dep_pkg$Version
#' download_pkg_sources(Package,Version)
#' }
download_pkg_sources <- function(Package,
                                 Version,
                                 lib_path = "devel/pkg_sources"){

  CRAN <- "https://cran.rstudio.com/src/contrib"
  #create output directory
  RJobTissueArea:::create_working_directory(lib_path)
  #replace _ in Version
  Version <- stringr::str_replace(Version,"_",".")
  #create package URL
  pkg_Folder <- paste0(Package,"_",Version,".tar.gz")
  URL <- paste0(CRAN,"/",pkg_Folder)
  #download
  e <- purrr::walk(URL,
                   ~try(
                     download.file(
                       url = .x,
                       destfile = paste0(file.path(lib_path,
                                                   basename(.x))))))
  # check what is present
  down_files <- list.files(lib_path)
  pos <- which(!pkg_Folder %in% down_files)
  # select packages to download from archieve
  URL_archive <- paste0(CRAN,"/Archive/",Package[pos],"/",pkg_Folder[pos])

  # then download
  e <- purrr::walk(URL_archive,
                   ~try(
                     download.file(
                       url = .x,
                       destfile = paste0(file.path(lib_path,
                                                   basename(.x))))))

  # check success
  down_files <- list.files(lib_path)

  # list of tar.gz files which needs manual download
  pos <- which(!pkg_Folder %in% down_files)

  writeLines(c("-packages remain to be downloaded manually:",
               Package[pos]))
}

#' create df of package dependencies
#'
#' @param exact_version character vector of package names where exact version is obligatory
#' @param ignore_folder character vector of folder names in pkg directory which are build-ignored
#' @param ignore_package character vector of package names to remove from final list
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' if(FALSE){
#' get_package_dependencies(
#' exact_version = c("mongolite","locfit","EBImage"),
#' ignore_folder <- c("devel"),
#' ignore_package <- "RJobTissueArea"
#'  )
#'  }
get_package_dependencies <- function(exact_version = "mongolite",
                                     ignore_folder = "devel",
                                     ignore_package = "RJobTissueArea"){
  #get all package dependencies
  deps<-renv::dependencies()
  #get all packages installed
  inst_pkg <- installed.packages()%>%
    as.data.frame()
  #add Installed Version of package dependencies
  deps <- dplyr::left_join(deps,
                           inst_pkg%>%
                             dplyr::select(Package,"InstalledVersion"="Version"),
                           by = "Package")
  #ignore folder
  ignore_pos <- purrr::map(ignore_folder,
                           ~stringr::str_detect(deps$Source,.x)%>%
                             which())%>%
    unlist()
  if(length(ignore_pos)>=1){
    deps <- deps[-ignore_pos,]
  }
  #summarize pkg and version
  deps_import <- deps%>%
    dplyr::group_by(Package)%>%
    dplyr::summarize(Version = unique(InstalledVersion))
  #filter pkg
  deps_import <- deps_import%>%
    dplyr::filter(Package != ignore_package)%>%
    dplyr::mutate(Version = as.character(Version))

  return(deps_import)

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


