# complete workflow - tissue detection
# includes imageProcessing on data_Sum
# mapping through group list
# investigate additional processing step in order to fill tissue black spots
# complete run on chipgroup and estimate tissue size
# filter BG and *

Version <- 170622

library(RJobTissueArea)

group_ID <- "P1761451"

output_dir <- "devel/data/data_output"
#output_dir_image_processing <- file.path(output_dir,"image_processing","extended_strategy")
input_dir_dataSum <- file.path(output_dir,
                               "image_processing",
                               "data_sum_collection")


create_working_directory(output_dir)
create_working_directory(input_dir_dataSum)


#sigma <- c(0.5,1,2,3,4)
#threshold <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
sigma <- c(1,15)
threshold <- c(1,2,5)
grid <- expand.grid(sigma=sigma,threshold=threshold)




chip_IDs <- find_valid_group_chip_IDs(group_ID)


#__________________
#create ScanHistory----

#ScanHistory <- create_ScanHistory_extended(chip_IDs,
#                                           output_dir,
#                                           result_ID = group_ID)
filename <- create_result_filepath(output_dir,
                                   "extendedScanHistory",
                                   group_ID,
                                   "csv")

ScanHistory <- data.table::fread(filename)

# create list of hdr images grouped by position_ID
# subsetting image data listed in ScanHistory
image_groups <- create_hdr_image_groups(ScanHistory,
                                        input_dir_dataSum)

# calculate data_sum for all image groups----
process_data_sum_for_image_groups(image_groups)


# apply tissue detection workflow----
process_TissueDetection(image_groups,
                        output_dir,
                        sigma = 15,
                        threshold = 2,
                        window = 10)






