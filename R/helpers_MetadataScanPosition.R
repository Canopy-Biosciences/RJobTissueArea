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
#'
#' @examples
#' data(EDL_chipIDs_limslager)
#' EDL <- EDL_chipIDs_limslager[1]
#' MH <- create_MethodHistory_from_EDL(EDL)
create_MethodHistory_from_EDL <- function(EDL){
function (EDL)
  V <- 210921
  if ((stringr::str_detect(EDL, "error_")) == TRUE) {
    return(EDL)
  }
  else {
    output <- try(EDL %>% xml2::read_xml() %>% xml2::xml_child("MethodHistory") %>%
                    xml2::xml_children() %>% purrr::map(xml2::xml_attrs) %>%
                    purrr::map_df(~as.list(.)), silent = TRUE)
    if (inherits(output, "try-error") == TRUE) {
      return(Error = ("error_no_MethodHistory in EDLchannel"))
    }
    else {
      return(output)
    }
  }
}

#' Title
#'
#' @param EDLs character vector containing EDL strings
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' data("EDL_chipIDs_limslager")
#' EDLs <- EDL_chipIDs_limslager
#' MHs <- create_MethodHistory_from_EDLs(EDLs)
create_MethodHistory_from_EDLs<-function(EDLs){

  # Version taken from Rmd report:
  # "!_final_Markdown_collect_FLvalues_allStains_180920_withCode_COMPLEMENT.Rmd"
  V <- 050722
  #updates
  #-  variable initiation
  #- map- vectorisation
  #- adding listNames

  output <- isTryError <- listNames<- NULL

  listNames <- names(EDLs)

  output<-purrr::map(EDLs,
                     ~try(.x%>%
                            xml2::read_xml()%>%
                            xml2::xml_child("MethodHistory")%>%
                            xml2::xml_children()%>%
                            purrr::map(xml2::xml_attrs)%>%
                            purrr::map_df(~as.list(.)),
                          silent = TRUE))

  isTryError <- purrr::map_lgl(output,
                               ~inherits(.x,'try-error'))

  if(any(isTryError)){
    output[isTryError] <- paste0("error_cant create a MethodHistory from EDL_",listNames[isTryError])
  }

  names(output) <- listNames

  return(output)
}



#' create_MethodHistory_of_chipIDs
#'
#' @param chip_IDs
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' \donttest{
#' data(chip_IDs)
#' MHs <- create_MethodHistory_of_chipIDs(chip_IDs)
#' }
create_MethodHistory_of_chipIDs <- function(chip_IDs){

  query_results <- query_UID_limslager(chip_IDs = chip_IDs)
  EDLs <- get_EDL_from_query_result(query_results)
  MethodHistory <- create_MethodHistory_from_EDLs(EDLs)

  return(MethodHistory)
}


#' create_ScanHistory_extended
#'
#' @param chip_IDs character of chip_IDs
#' @param output_dir character of output directory
#' @param result_ID character of added to filename containing the result df
#'
#' @return a dataframe and a csv file exported into output_dir
#' @export
#'
#' @examples
#' \donttest{
#' data("chip_IDs")
#' output_dir <- tempdir()
#' result_ID <- "test"
#' SH_ext <- create_ScanHistory_extended(chip_IDs,output_dir,result_ID)
#' }
create_ScanHistory_extended <- function(chip_IDs,output_dir,result_ID){

  Version <- "050722"
  # UPDATE
  #- select_valid_image_files call included
  #- add sampleType

  tictoc::tic("create extended ScanHistory")

  # query chip_IDs limslager----
  query_results <- query_UID_limslager(chip_IDs = chip_IDs)

  # get EDL_chip_limslager----
  EDLs <- get_EDLs_of_queryResult(query_results)

  MethodHistorys <- create_MethodHistory_from_EDLs(EDLs)

  # create scanHistory-----
  ScanHistorys = create_ScanHistory_of_MethodHistory(MethodHistorys)

  #get_sampleType----
  sampleTypes <- purrr::map_chr(MethodHistorys,
                              ~get_sampleType_from_MethodHistory(.x))

  # add filterset (query limsproc)
  ScanHistory1 <- ScanHistorys%>%
    dplyr::mutate(filterset = query_filterset_of_scanIDs(scan_IDs = ScanHistorys$scan_ID))

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
  ScanHistory2 <- dplyr::full_join(ScanHistorys,
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


  # add sampleType----
  ScanHistory8 <- dplyr::left_join(ScanHistory7,
                                  data.frame(sampleType = sampleTypes,
                                             chip_ID = names(sampleTypes)),
                                  by = "chip_ID")
  # export ScanHistory
  result_filename <- create_result_filepath(output_dir,
                                            "extendedScanHistory",
                                            result_ID,
                                            "csv")

  data.table::fwrite(ScanHistory8,result_filename)

  tictoc::toc()
  return(ScanHistory8)

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
#' \donttest{
#' data(chip_IDs)
#' SHs <- create_ScanHistory_of_chipIDs(chip_IDs)
#' }
create_ScanHistory_of_chipIDs<-function(chip_IDs){

  Version <- "290422"
  #update:
  #- purrr::map_df
  MethodHistory <- create_MethodHistory_of_chipIDs(chip_IDs)

  ScanHistorys <- create_ScanHistory_of_MethodHistory(MethodHistory)
  #purrr::map_df(MethodHistory,
  #                              ~.x%>%
  #                                dplyr::rename("scan_ID" = "UID")%>%
  #                                dplyr::rename("cycle_ID" = "CycleUID")%>%
  #                                tidyr::fill(cycle_ID,  .direction = "up")%>%
  #                                dplyr::filter(Type == "Chipcytometry-Scan")%>%
  #                                dplyr::select(scan_ID,cycle_ID,Status,Tag,Excluded,PreparedForDataviz))
  return(ScanHistorys)
}


#' create_ScanHistory_of_MethodHistory
#'
#' @param MethodHistory
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' data("EDL_chipIDs_limslager")
#' EDLs <- EDL_chipIDs_limslager
#' MHs <- create_MethodHistory_from_EDLs(EDLs)
#' SHs <- create_ScanHistory_of_MethodHistory(MHs)
create_ScanHistory_of_MethodHistory<-function(MethodHistory){

  Version <- "290422"
  #update:
  #- purrr::map_df

  ScanHistorys <- purrr::map_df(MethodHistory,
                                ~.x%>%
                                  dplyr::rename("scan_ID" = "UID")%>%
                                  dplyr::rename("cycle_ID" = "CycleUID")%>%
                                  tidyr::fill(cycle_ID,  .direction = "up")%>%
                                  dplyr::filter(Type == "Chipcytometry-Scan")%>%
                                  dplyr::select(scan_ID,cycle_ID,Status,Tag,Excluded,PreparedForDataviz))
  return(ScanHistorys)
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
#' data("EDL_chipIDs_limslager")
#' EDL <- EDL_chipIDs_limslager[1]
#' MH <- create_MethodHistory_from_EDL(EDL)
#' sampleType <- get_sampleType_from_MethodHistory(MH)
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
