select_hdr_files <-
function (result_files) 
{
    hdr_files <- result_files %>% dplyr::filter(!is.na(hdr_filename)) %>% 
        dplyr::filter(!Tag %in% c("BG", "*")) %>% dplyr::mutate(hdr_filepath = create_hdr_filepath(chip_path, 
        scan_ID, pos_ID))
    return(hdr_files)
}
select_valid_image_files <-
function (result_files, type = NULL) 
{
    Version <- "290422"
    type <- match.arg(type, choices = c("none", "blob", "blob32", 
        "png"))
    result_files <- result_files %>% dplyr::filter(Excluded %in% 
        c("FALSE")) %>% dplyr::filter(Status == "Finished")
    if (type == "blob") {
        result_files <- result_files %>% dplyr::filter(filetype == 
            "blob")
    }
    if (type == "blob32") {
        result_files <- result_files %>% dplyr::filter(filetype == 
            "blob32")
    }
    if (type == "png") {
        result_files <- result_files %>% dplyr::filter(filetype == 
            "png")
    }
    if (1 %in% result_files$enabled) {
        result_files <- result_files %>% dplyr::filter(enabled == 
            1)
    }
    return(result_files)
}
create_hdr_filepath <-
function (chip_path, scan_ID, pos_ID) 
{
    pos <- which(pos_ID %in% c(1:9))
    pos_ID[pos] <- paste0("0", pos_ID[pos])
    hdr_file_path <- file.path(chip_path, "scanjobs", scan_ID, 
        paste0("pos", pos_ID), "hdr")
}
find_chip_path <-
function (ChannelID = "M583054", server_path = NULL) 
{
    if (is.null(server_path)) {
        server_path <- find_server_path()$server_path
    }
    if (any(c(!is.character(ChannelID), !is.vector(server_path), 
        length(ChannelID) != 1))) {
        error_text <- paste0("Cannot find chip path: ", ChannelID)
        stop_if_fatal(error_text)
    }
    else {
        if (!any(file.exists(server_path))) {
            return("error_server do not exist under the provided address")
        }
        else {
            if (any(stringr::str_detect(ChannelID, "error_"))) {
                return(ChannelID)
            }
            else {
                chip_path <- NA
                check_file <- FALSE
                n_server <- length(server_path)
                i <- 1
                while ((!check_file) & i <= n_server) {
                  chip_path <- paste0(server_path[i], "\\", ChannelID) %>% 
                    file.path()
                  check_file <- file.exists(chip_path)
                  i <- i + 1
                }
                if (check_file == FALSE) {
                  error_text <- "error_no path to chip data found on the provided servers."
                  logger::log_debug(paste0(error_text, ": ", 
                    "{chip_path}"))
                  chip_path <- error_text
                }
                else {
                  logger::log_debug("Successfully found chip path: {chip_path}")
                }
                return(chip_path)
            }
        }
    }
}
find_server_path <-
function () 
{
    name_result <- error <- server_names <- error_text <- NULL
    names_result <- query_mongoDB(mongo_collection = "limslager", 
        attribute_name = "EDLType", attribute_value = "ImageServerPath")
    error <- names_result$error_message
    if (rlang::is_empty(error)) {
        server_names <- names_result$result[[1]]
        server_names <- server_names %>% dplyr::select(.data$FlagEmpty, 
            .data$EDLName, .data$EDL) %>% dplyr::filter(.data$FlagEmpty == 
            0)
        server_names <- server_names %>% dplyr::mutate(server_path = purrr::map_chr(.data$EDL, 
            ~.x %>% xml2::read_xml() %>% xml2::xml_find_all("//SpecificParameter[@Name='StoragePath']") %>% 
                xml2::xml_attr("Value"))) %>% dplyr::select(-.data$EDL)
    }
    else {
        error_text <- "Cannot find server names."
        stop_if_fatal(error_text)
    }
    return(server_names)
}
get_enabled_positions <-
function (chip_ID) 
{
    query_result <- query_chipID_channels(chip_IDs)
    positions_list <- get_positions_field_from_query_result(query_result)
    enabled_positions <- get_enabled_positions_from_positions_list(positions_list)
    return(enabled_positions)
}
query_filterset_of_scanIDs <-
function (scan_IDs) 
{
    query_scan_scans <- query_UID_limsproc(scan_IDs)
    EDL <- get_EDL_from_query_result(query_scan_scans)
    filterset <- purrr::map_chr(EDL, ~.x %>% xml2::read_xml() %>% 
        xml2::xml_find_all("/Method/Machine/SpecificParameters/SpecificParameter[@Name = \"Active Filterset-ID\"]") %>% 
        xml2::xml_attr("Value"))
    return(filterset)
}
query_mongoDB <-
function (mongo_collection, attribute_name, attribute_value) 
{
    error <- NULL
    mongo_connection <- NULL
    query_string <- NULL
    query_result <- NULL
    no_result <- NULL
    result <- NULL
    message <- NULL
    error_message <- NULL
    error_check <- try(params::check_args(select = c(mongo_collection, 
        attribute_name, attribute_value)), silent = TRUE)
    if (inherits(error_check, "try-error")) {
        cat(paste0("error_missing input: ", "\n", error_check[1], 
            " - process interupted !!!"))
    }
    else {
        mongo_connection <- connect_mongoDB(mongo_collection)
        if (!inherits(mongo_connection, "try-error")) {
            query_string <- paste0("'{\"", attribute_name, "\":\"", 
                attribute_value, "\"}'")
            query_result <- purrr::map(query_string, ~mongo_connection$find(query = eval(parse(text = .x))))
            no_result <- purrr::map_lgl(query_result, ~rlang::is_empty(.x))
            result <- query_result[which(no_result == FALSE)]
            message <- paste0(attribute_name, "_", attribute_value, 
                "_", mongo_collection)[which(no_result == FALSE)]
            result <- purrr::map2(result, message, ~.x %>% dplyr::mutate(query = .y))
            error_message <- paste0("error_no EDL found in ", 
                mongo_collection, " for ", attribute_name, "_", 
                attribute_value)
            error_message <- error_message[which(no_result)]
            return(list(result = result, error_message = error_message))
        }
    }
}
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
create_result_filepath <-
function (output_dir, name_string, result_ID, type) 
{
    path <- file.path(output_dir, paste0(name_string, "_", result_ID, 
        ".", type))
    return(path)
}
