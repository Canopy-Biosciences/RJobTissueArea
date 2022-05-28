library(targets)
options(crayon.enabled = FALSE)
tar_option_set(memory = "transient", garbage_collection = TRUE)
tar_option_set(packages = c("RJobTissueArea", "tibble", "readr", 
    "dplyr", "ggplot2"), imports = "RJobTissueArea")
extract_chipIDs_from_groupEDL <- function(EDL) {
    chip_IDs <- EDL %>% xml2::read_xml() %>% xml2::xml_find_all("/Obj/EncapsulatedObjectsRef/ObjRef") %>% 
        xml2::xml_attr("UID")
    return(chip_IDs)
}
list(tar_target(group_ID, "P1761451"), tar_target(query_result, 
    query_mongoDB("limslager", "UID", group_ID)), tar_target(EDL, 
    query_result %>% get_EDL_from_query_result()), tar_target(chip_IDs, 
    EDL %>% extract_chipIDs_from_groupEDL()), tar_target(valid_chipIDs, 
    chip_IDs %>% check_if_chip_data_exist()))
