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
get_enabled_positions_from_positions_list <-
function (positions_list) 
{
    enabled_positions <- tibble::tibble(chip_ID = positions_list$chip_ID, 
        enabled_positions = purrr::map(positions_list$positions, 
            ~.x %>% extract_enabled_positions())) %>% tidyr::unnest(cols = c("enabled_positions"))
}
extract_enabled_positions <-
function (single_pos_entity) 
{
    df <- data.frame(pos_ID = single_pos_entity["posid"], enabled = single_pos_entity["enabled"])
    return(df)
}
read_binary_image_as_vector <-
function (image_path, blob_filename) 
{
    Version <- "030522"
    blob_parameter <- read_XML_BLOB_parameter(image_path, blob_filename)
    data <- read_image_binary_file(blob_parameter)
    return(data)
}
read_image_binary_file <-
function (blob_parameter) 
{
    Version <- "270422"
    encoding <- blob_parameter %>% extract_encoding_from_blob_parameter()
    bin_size <- encoding %>% convert_binsize_from_encoding()
    n_pixels <- blob_parameter %>% extract_n_pixels_from_blob_parameter()
    path <- blob_parameter %>% extract_image_path_from_blob_parameter()
    data <- readBin(path, integer(), n = n_pixels, size = bin_size)
    attr(data, "h_pixel") <- extract_h_pixels_from_blob_parameter(blob_parameter)
    attr(data, "v_pixel") <- extract_v_pixels_from_blob_parameter(blob_parameter)
    attr(data, "image_resolution") <- extract_image_resolution_from_blob_parameter(blob_parameter)
    return(data)
}
read_XML_BLOB_parameter <-
function (image_path, blob_filename) 
{
    Version <- "260422"
    path <- file.path(image_path, paste0(blob_filename, ".xml"))
    XML <- path %>% xml2::read_xml()
    XML_parameter <- XML %>% create_long_node_df_from_XML()
    attr(XML_parameter, "image_path") <- image_path
    attr(XML_parameter, "blob_filename") <- blob_filename
    attr(XML_parameter, "xml_path") <- path
    return(XML_parameter)
}
extract_image_resolution_from_blob_parameter <-
function (blob_parameter) 
{
    Version <- "270422"
    v_pixel <- extract_v_pixels_from_blob_parameter(blob_parameter)
    width <- extract_image_width_from_blob_parameter(blob_parameter)
    v_res <- width/v_pixel
    h_pixel <- extract_h_pixels_from_blob_parameter(blob_parameter)
    heigth <- extract_image_heigth_from_blob_parameter(blob_parameter)
    h_res <- heigth/h_pixel
    resolution_unit <- "[um per pixel edge]"
    image_resolution <- c(h_res, v_res)
    attr(image_resolution, "resolution_unit") <- resolution_unit
    return(image_resolution)
}
extract_n_pixels_from_blob_parameter <-
function (blob_parameter) 
{
    Version <- "270422"
    h_pixel <- extract_h_pixels_from_blob_parameter(blob_parameter)
    height <- extract_v_pixels_from_blob_parameter(blob_parameter)
    n_pixels <- h_pixel * height
    return(n_pixels)
}
extract_v_pixels_from_blob_parameter <-
function (blob_parameter) 
{
    Version <- "270422"
    v_pixel <- blob_parameter %>% dplyr::filter(node_name == 
        "size") %>% dplyr::filter(node_attributes_id == "height") %>% 
        dplyr::pull(node_attributes) %>% as.numeric()
    return(v_pixel)
}
extract_h_pixels_from_blob_parameter <-
function (blob_parameter) 
{
    Version <- "270422"
    h_pixel <- blob_parameter %>% dplyr::filter(node_name == 
        "size") %>% dplyr::filter(node_attributes_id == "width") %>% 
        dplyr::pull(node_attributes) %>% as.numeric()
    return(h_pixel)
}
extract_encoding_from_blob_parameter <-
function (blob_parameter) 
{
    Version <- "270422"
    encoding <- blob_parameter %>% dplyr::filter(node_attributes_id == 
        "encoding") %>% dplyr::pull(node_attributes)
    return(encoding)
}
convert_binsize_from_encoding <-
function (encoding) 
{
    Version <- "270422"
    bin_size <- dplyr::case_when(encoding == "32bit little-endian" ~ 
        4, encoding == "16bit little-endian" ~ 2)
    return(bin_size)
}
extract_image_path_from_blob_parameter <-
function (blob_parameter) 
{
    Version <- "270422"
    path <- file.path(attr(blob_parameter, "image_path"), attr(blob_parameter, 
        "blob_filename"))
    return(path)
}
extract_image_width_from_blob_parameter <-
function (blob_parameter) 
{
    Version <- "270422"
    width <- blob_parameter %>% dplyr::filter(node_name == "extent") %>% 
        dplyr::filter(node_attributes_id == "width") %>% dplyr::pull(node_attributes) %>% 
        as.numeric()
    return(width)
}
extract_image_heigth_from_blob_parameter <-
function (blob_parameter) 
{
    Version <- "270422"
    heigth <- blob_parameter %>% dplyr::filter(node_name == "extent") %>% 
        dplyr::filter(node_attributes_id == "height") %>% dplyr::pull(node_attributes) %>% 
        as.numeric()
    return(heigth)
}
