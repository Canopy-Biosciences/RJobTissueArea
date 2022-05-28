create_ScanHistory_of_chipIDs <-
function (chip_IDs) 
{
    Version <- "290422"
    MethodHistory <- create_MethodHistory_of_chipIDs(chip_IDs)
    ScanHistorys <- purrr::map_df(MethodHistory, ~.x %>% dplyr::rename(scan_ID = "UID") %>% 
        dplyr::rename(cycle_ID = "CycleUID") %>% tidyr::fill(cycle_ID, 
        .direction = "up") %>% dplyr::filter(Type == "Chipcytometry-Scan") %>% 
        dplyr::select(scan_ID, cycle_ID, Status, Tag, Excluded, 
            PreparedForDataviz))
    return(ScanHistorys)
}
create_MethodHistory_of_chipIDs <-
function (chip_IDs) 
{
    query_results <- query_UID_limslager(chip_IDs = chip_IDs)
    EDLs <- get_EDL_from_query_result(query_results)
    MethodHistory <- purrr::map(EDLs, ~create_MethodHistory_from_EDL(.x))
    return(MethodHistory)
}
create_MethodHistory_from_EDL <-
function (EDL) 
{
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
