# complete workflow - imageProcessing - data SumApproach - map group list
Version <- 030522

library(RJobTissueArea)
output_dir <- "devel/data/data_output"
group_ID <- "P1761451"
create_working_directory(output_dir)
chip_IDs <- find_valid_group_chip_IDs(group_ID)
#ScanHistory <- create_ScanHistory_extended(chip_IDs,
#                                           output_dir,
#                                           result_ID = group_ID)
filename <- create_result_filepath(output_dir,
                                   "extendedScanHistory",
                                   group_ID,
                                   "csv")

ScanHistory <- data.table::fread(filename)
result_files <- select_valid_image_files(ScanHistory,type = NULL)
hdr_files <- select_hdr_files(result_files)
image_groups <- hdr_files%>%
  dplyr::rename("image_path"="hdr_filepath",
                "blob_filename"="hdr_filename")%>%
  dplyr::group_by(chip_ID,pos_ID)%>%
  tidyr::nest()%>%
  dplyr::mutate(group_ID = paste0(chip_ID,"_",pos_ID))%>%
  dplyr::mutate(data_file = create_result_filepath(output_dir,
                                                   "data_sum",
                                                   group_ID,
                                                   type="csv"))

sigma <- c(0.5,1,2,3,4)
threshold <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
grid <- expand.grid(sigma=sigma,threshold=threshold)

output_dir <- file.path("devel/data/data_output/image_processing")

for(j in 1:dim(image_groups)[1]){


  image_group_list <- image_groups$data[[j]]
  group_ID <-  image_groups$group_ID[j]

  for(i in 1:dim(image_group_list)[1]){

    image_path <- image_group_list$image_path[i]
    blob_filename <- image_group_list$blob_filename[i]

    data_mat <- read_binary_image_as_matrix(image_path,
                                            blob_filename)
    data <- read_binary_image_as_vector(image_path,
                                        blob_filename)

    if(i == 1){
      data_sum <- data
    }else{

      if(any(attr(data_mat,"image_resolution") != attr(data_sum,"image_resolution"))){
        writeLines(c(
          paste0("- image_resolution changed with scan_ID: ", image_group_list$scan_ID[i])
        ))}

      data_sum <- data_sum+data

    }
  }

  m_data_sum <-matrix(data,
                      ncol=attr(data_sum,"h_pixel"),
                      nrow=attr(data_sum,"v_pixel"),
                      byrow=TRUE)

  cellres <- as.vector(attr(data_sum,"image_resolution"))
  chip_ID <- image_groups$chip_ID[j]
  pos_ID <- image_groups$pos_ID[j]

  saveRDS(m_data_sum,
          file.path(output_dir,
                    "data_sum_collection_3",
                    paste0("dataSum_",chip_ID,"_",pos_ID,".rds")))



  for(k in 1:dim(grid)[1]){

    output_dir_grid <- file.path(output_dir,paste(grid$sigma[k],"_",grid$threshold[k]))

    perform_image_processing(m_data_sum,
                             cellres = cellres,
                             output_dir = output_dir_grid,
                             chip_ID,
                             pos_ID,
                             sigma = grid$sigma[k],
                             threshold = grid$threshold[k])
  }

}
#image <-pixmapGrey(m_data_sum,cellres=c(1,1))
#plot(image)

