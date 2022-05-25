V <- "250522"
helpers <- "MetadataScanPosition"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_MethodHistory_from_EDL()",
    "- create_MethodHistory_of_chipIDs()",
    "- create_ScanHistory_of_chipIDs()",
    "- create_ScanHistory_extended()",
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
    "- select_segment_ID()"
  ))

#' Title
#'
#' @param EDL
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
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



#' create_MethodHistory_of_chipIDs
#'
#' @param chip_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
create_MethodHistory_of_chipIDs <- function(chip_IDs){

  query_results <- query_UID_limslager(chip_IDs = chip_IDs)
  EDLs <- get_EDL_from_query_result(query_results)
  MethodHistory <- purrr::map(EDLs,~create_MethodHistory_from_EDL(.x))

  return(MethodHistory)
}

#' create_ScanHistory_of_chipIDs
#'
#' @param chip_IDs
#'
#' @return
#' @export
#' @keywords internal
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
#' @param output_dir
#' @param result_ID
#'
#' @return
#' @export
#'
#' @examples
create_ScanHistory_extended <- function(chip_IDs,
                                        output_dir,
                                        result_ID){

  Version <- "290422"

  tictoc::tic("create extended ScanHistory")

  #_____________________________________
  # create scanHistory (query limslager)----
  ScanHistory = create_ScanHistory_of_chipIDs(chip_IDs)%>%
    data.table::rbindlist()

  #_______________________________
  # add filterset (query limsproc)----
  ScanHistory <- ScanHistory%>%
    dplyr::mutate(filterset = query_filterset_of_scanIDs(scan_ID))

  #______________________________
  # query result chipID ins scans----
  query_chip_scans <- query_mongoDB("scans",
                                    "channelUID",
                                    chip_IDs)

  #_______________
  # select columns----
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

  #_________________
  # join ScanHistory----
  ScanHistory2 <- dplyr::full_join(ScanHistory,
                                   results_chip_scans, by = "scan_ID")

  #___________________________
  # subselect columns position----
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

  #____________________________________
  #unnest selected columns in positions----
  ScanHistory4 <- ScanHistory3%>%
    tidyr::unnest(positions)

  #________________________________
  # extract hdr column in positions----
  ScanHistory5 <- ScanHistory4%>%
    dplyr::mutate(hdr_filename = purrr::map(ScanHistory4$hdr,~.x)$filename)%>%
    dplyr::select(-hdr)

  # Image_list <- ScanHistory4%>%
  #   dplyr::select(chip_ID,scan_ID,pos_ID,hdr,flimages,focus,deltaTL,posref)


  #__________________________________________
  # get enabled positions (query in channels)----
  enabled_positions <- get_enabled_positions(chip_IDs)

  #___________________________
  # join enabled position flag----
  ScanHistory6<- dplyr::left_join(ScanHistory5,
                                  enabled_positions,
                                  by=c("chip_ID", "pos_ID"="posid"))

  #_____________
  #add chip_path----
  serverpath <- find_server_path()

  chip_paths <- data.frame(
    chip_ID = chip_IDs,
    chip_path = purrr::map_chr(chip_IDs,
                               ~find_chip_path(.x)))

  ScanHistory7 <- ScanHistory6%>%
    dplyr::left_join(chip_paths,by="chip_ID")

  #___________________
  # export ScanHistory----
  result_filename <- create_result_filepath(output_dir,
                                            "extendedScanHistory",
                                            result_ID,
                                            "csv")
  data.table::fwrite(ScanHistory7,result_filename)

  tictoc::toc()

  return(ScanHistory7)

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

