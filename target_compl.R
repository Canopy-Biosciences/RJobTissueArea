library(targets)
options(crayon.enabled = FALSE)
tar_option_set(memory = "transient", garbage_collection = TRUE)
tar_option_set(packages = c("RJobTissueArea", "tibble", "readr", 
    "dplyr", "ggplot2"), imports = "RJobTissueArea")
Sys.setenv(TAR_WARN = "false")
tar_config_set(store = file.path("inst", "analysis_workflow"), 
    script = "target_compl.R")
mapping_imagelist_calculate_data_sum <- function(image_groups) {
    purrr::map(image_groups, ~calculate_data_sum(.x))
}
list(tar_target(group_ID, "P1761451"), tar_target(output_dir, 
    "devel/data/data_output"), tar_target(grid, expand.grid(sigma = c(1, 
    15), threshold = c(1, 2, 5))), tar_target(chip_IDs, data(chip_IDs)), 
    tar_target(ScanHistory, create_ScanHistory_extended(chip_IDs, 
        output_dir, group_ID)), tar_target(filename, create_result_filepath(output_dir, 
        "extendedScanHistory", group_ID, "csv")), tar_target(ScanHistory2, 
        data.table::fread(filename)), tar_target(image_groups, 
        create_hdr_image_groups(ScanHistory)), tar_target(data_sum, 
        mapping_imagelist_calculate_data_sum(image_groups)))
