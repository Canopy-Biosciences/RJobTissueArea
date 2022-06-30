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
    "- create_ScanHistory_extended()",
    "- create_ScanHistory_of_chipIDs()"
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


create_ScanHistory_extended <- function(chip_IDs,output_dir,result_ID){

  Version <- "250522"
  # UPDATE
  #- select_valid_image_files call included

  tictoc::tic("create extended ScanHistory")

  # create scanHistory (query limslager)
  ScanHistory = create_ScanHistory_of_chipIDs(chip_IDs)

  # add filterset (query limsproc)
  ScanHistory1 <- ScanHistory%>%
    dplyr::mutate(filterset = query_filterset_of_scanIDs(scan_IDs = ScanHistory$scan_ID))

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

  # join ScanHistory and select valid entities
  ScanHistory2 <- dplyr::full_join(ScanHistory,
                                   results_chip_scans, by = "scan_ID")

  ScanHistory0 <- ScanHistory2%>%
    select_valid_image_files()

  # subselect columns position
  ScanHistory3 <- ScanHistory0%>%
    dplyr::mutate(positions = purrr::map(
      ScanHistory0$positions,
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


