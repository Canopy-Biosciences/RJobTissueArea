library(targets)
options(crayon.enabled = FALSE)
tar_option_set(memory = "transient", garbage_collection = TRUE)
tar_option_set(packages = c("RJobTissueArea", "tibble", "readr",
    "dplyr", "ggplot2"), imports = "RJobTissueArea")
Sys.setenv(TAR_WARN = "false")
tar_config_set(store= file.path("inst", "analysis_workflow"),script =  "target_compl.R")
mapping_imagelist_calculate_data_sum <- function(image_groups) {
}
list(tar_target(group_ID, groupID), tar_target(output_dir, outputdir),
    tar_target(grid, expand.grid(sigma = sigma, threshold = threshold)),
    tar_target(chip_IDs, find_valid_group_chip_IDs(group_ID)),
    tar_target(ScanHistory, create_ScanHistory_extended(chip_IDs,
        output_dir, group_ID)), tar_target(filename, create_result_filepath(output_dir,
        "extendedScanHistory", group_ID, "csv")), tar_target(ScanHistory2,
        data.table::fread(filename)), tar_target(image_groups,
        create_hdr_image_groups(ScanHistory)), tar_target(data_sum,
        process_data_sum_for_image_groups(image_groups)))
