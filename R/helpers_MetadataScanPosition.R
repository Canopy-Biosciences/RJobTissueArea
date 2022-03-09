V <- "090322"
helpers <- "MetadataScanPosition"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_MethodHistory_of_chipIDs()",
    "- create_ScanHistory_of_chipIDs()",
    "- create_pos_df_from_MethodHistory()",
    "- extract_cycle_sequence_from_MethodHistory()",
    "- extract_OK_pos_from_MethodHistory()",
    "- extract_Ref_pos_from_MethodHistory()",
    "- extract_XYvalues_pos_from_MethodHistory()",
    "- get_pos_colnames_in_MethodHistory()"
  ))


#' create_MethodHistory_of_chipIDs
#'
#' @param chip_IDs
#'
#' @return
#' @export
#'
#' @examples
create_MethodHistory_of_chipIDs <- function(chip_IDs){

  query_results <- query_UID_limslager(chip_IDs = chip_IDs)
  EDLs <- get_EDL_from_query_result(query_results)
  MethodHistory <- purrr::map(EDLs,~create_MethodHistory_from_EDL(.x))

  return(MethodHistory)
}

#' create_pos_df_from_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
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

#' create_ScanHistory_of_chipIDs
#'
#' @param MethodHistory
#'
#' @return
#' @export
#'
#' @examples
create_ScanHistory_of_chipIDs<-function(MethodHistory){
  ScanHistorys <- purrr::map(MethodHistory,
                             ~.x%>%
                               dplyr::rename("scan_ID" = "UID")%>%
                               dplyr::rename("cycle_ID" = "CycleUID")%>%
                               tidyr::fill(cycle_ID,  .direction = "up")%>%
                               dplyr::filter(Type == "Chipcytometry-Scan")%>%
                               dplyr::select(scan_ID,cycle_ID,Status,Tag,Excluded,PreparedForDataviz))
  return(ScanHistorys)
}

#' extract_cycle_sequence_from_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
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



#' get_pos_colnames_in_MethodHistory
#'
#' @param MH
#'
#' @return
#'
#' @examples
get_pos_colnames_in_MethodHistory <- function(MH){
  cols <- colnames(MH)
  col_pos <- which(stringr::str_detect(cols,"pos")==TRUE)
  col_scanID <- which(cols %in% c("UID","CycleUID","Taq","Excluded","PreparedForDataviz"))
  return(c(col_scanID,col_pos))
}
