## code to prepare `imageProcessing` dataset

data("chip_IDs")
chip_ID <- chip_IDs[1]

output_dir <- "data-raw"
ScanHistory <- create_ScanHistory_extended(chip_IDs,
                                           output_dir,
                                           result_ID = group_ID)

image_groups <- create_hdr_image_groups(ScanHistory)

j=1
image_group_list <- image_groups$data[[j]]
group_ID <-  image_groups$group_ID[j]
chip_ID <- image_groups$chip_ID[j]
pos_ID <- image_groups$pos_ID[j]









sigma = 15
threshold = 35
window = 10
attenuation = 0.01
noiseReduction = TRUE
plot_image = TRUE
export_result = TRUE
result_ID = ""





usethis::use_data(image_group_list, overwrite = TRUE)






save(ScanHistory,
     image_groups,
     file="data-raw/SysDataRaw_ImageProcessing")
load("data-raw/SysDataRaw_ImageProcessing")
