# complete workflow - imageProcessing - data SumApproach - map group list
# apply image processing
# investigate additional processing step in order to fill tissue black spots
# complete run on chipgroup and estimate tissue size

Version <- 080522

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

#sigma <- c(0.5,1,2,3,4)
#threshold <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
sigma <- c(1,15)
threshold <- c(1,2,5)
grid <- expand.grid(sigma=sigma,threshold=threshold)


#output_dir_image_processing <- file.path(output_dir,"image_processing","extended_strategy")
input_dir_dataSum <- file.path(output_dir,"image_processing", "data_sum_collection_3")

#create result_df----

result_df <- tibble::tibble(
  group_ID = character(0), # group_ID
  chip_ID = character(0), #chip_ID
  pos_ID = numeric(0), #pos_ID
  InputImageFile = character(0), # m_data_file
  sigma = numeric(0),
  threshold = numeric(0),
  GS_window = numeric(0),
  n_pixel = double(0),
  n_tissue_pixel = double(0)
)
#__________________________
#1) map through image group----

j=1
for(j in 1:dim(image_groups)[1]){


  image_group_list <- image_groups$data[[j]]
  group_ID <-  image_groups$group_ID[j]
  chip_ID <- image_groups$chip_ID[j]
  pos_ID <- image_groups$pos_ID[j]

  #_________________
  #__create data_sum----
  i=1
  for(i in 1:dim(image_group_list)[1]){

    image_path <- image_group_list$image_path[i]
    blob_filename <- image_group_list$blob_filename[i]

    #data_mat <- read_binary_image_as_matrix(image_path,
    #                                        blob_filename)
    data <- read_binary_image_as_vector(image_path,
                                        blob_filename)

    if(i == 1){
      data_sum <- data
    }else{

      if(any(attr(data,"image_resolution") != attr(data_sum,"image_resolution"))){
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

  #_________________
  #__export data_sum----
  create_working_directory(file.path(output_dir,
                                     "image_processing",
                                     "data_sum_collection_3"))
  saveRDS(m_data_sum,
          file.path(output_dir,
                    "image_processing",
                    "data_sum_collection_3",
                    paste0("dataSum_",chip_ID,"_",pos_ID,".rds")))

  #__________________
  #__read in data_sum----

  #m_data_file <- file.path(input_dir_dataSum,
  #                         paste0("dataSum_",chip_ID,"_",pos_ID,".rds"))
#
  #m_data_sum <- readRDS(m_data_file)
  ##cellres <- as.vector(attr(m_data_sum,"image_resolution"))
#
  #_____________________
  #__2) map through grid----
  k=1
  m.data <- m_data_sum
  cellres <- c(1,1)

  for(k in 1:dim(grid)[1]){
    sigma = grid$sigma[k]
    threshold = grid$threshold[k]
    Version <- "080522"

    #_________________
    #___create pixmap----
    # cellres: pixel resolution in horizontal and vertical direction

    image <-pixmap::pixmapGrey(m.data,
                               cellres=cellres)

    #___________________
    #___get grey values----
    grey_values <- image@grey * 255

    #____________________________
    #___Low-pass Gaussian filter----
    #remotes::install_version("locfit",version="1.5.9.4")
    #BiocManager::install("EBImage",force=TRUE)
    #EBImage version: 4281
    xb <- EBImage::gblur(grey_values,
                         sigma)

    #________________
    #___round values----
    xb <- round(xb,digits = 1)

    #_______________________________
    #___create blurred pixmap image----
    image_blurred <- pixmap::pixmapGrey(xb,
                                        cellres=cellres)

    #______________________
    #___threshold filtering----
    pos <- which(xb > threshold)
    xt <- xb
    xt[which(xb > threshold)] <- 1
    xt[which(xb <= threshold)] <- 0

    #________________________________
    #___pixmap object of binary image----
    image_binary <- image
    image_binary@grey <- xt

    #__________________
    #___adapting window----
    xta <- EBImage::thresh(xt, w=1,h=1)

    image_adaptedThreshold <- pixmap::pixmapGrey(xta,
                                                 cellres=cellres)

    #_____________________________
    #___3) map throgh shink_vector----

    GS <- c(10,50)
    gs <- 1
    for(gs in 1:length(GS)){

      window <- GS[gs]

      #____________________________
      #____convert to imager object----

      imager_pxset <- imager::as.cimg(image_binary@grey)

      #________________
      #____ grow binary----
      xg <- imager::grow(imager_pxset, GS[gs],GS[gs],GS[gs])


      #_________________
      #____shrink binary----
      xs <-imager::shrink(xg,GS[gs],GS[gs],GS[gs])

      #____count tissue pixel----
      n_tissue_pixel <- which(xs == 1)%>%length()
      n_pixel <- xs%>%length()

      #________________________________
      #____add pixel count to result_df----

      result_df <- result_df %>%
        tibble::add_row(group_ID = group_ID,
                        chip_ID = chip_ID,
                        pos_ID = pos_ID,
                        InputImageFile = m_data_file,
                        sigma = sigma,
                        threshold = threshold,
                        GS_window = window,
                        n_pixel = n_pixel,
                        n_tissue_pixel = n_tissue_pixel
        )

      #___________________________
      #____create output directory----

      output_dir_image_processing <- file.path(
        output_dir,
        "image_processing","extended",
        paste0("blurring_sigma_",sigma),
        paste0("thresholding_values_",threshold),
        paste0("growANDshinking_windowsize_",window)
        )

      create_working_directory(output_dir_image_processing)

      #_______________________
      #____create output plots----
      # complete filepath

      file <- create_result_filepath(
        output_dir_image_processing,
        "ResultImages",
        paste0(chip_ID,"_",pos_ID,"_",sigma,"_",threshold,"_",window),
      #  "tiff"
      "png"
      #"jpeg"
      )

      # create tiff object
      #tiff(filename = file,
       #    width = 1400*5,
        #   height = 1400)

      png(filename = file, width = 1400*3, height = 1400)
      par(mfrow=c(1,3))

      # original convert to imager object and plot
      imager::as.cimg(image@grey)%>%plot(main = "original dataSum")

      # binary convert to imager object and plot
      imager::as.cimg(image_binary@grey)%>%
        plot(main= paste0("sigma set to: ",sigma," - threshold set to: ",threshold))

      # second adapting threshold convert to imager object and plot
     # imager::as.cimg(image_adaptedThreshold@grey)%>%
    #    plot(main= paste0("threshold window width set to: ",window))

      # growed images
      #xg %>% plot(main = paste0("grow by: ",GS[gs]," pixel"))

      # shrinked image
      xs %>% plot(main=paste0("shrink by: ",GS[gs]," pixel"),
                  sub = paste0(n_tissue_pixel/n_pixel*100," [%tissueArea]"))

      dev.off()
    }
  }
}

#________________
#export result_df----
create_working_directory(file.path(output_dir,
                                   "image_processing",
                                   "extended"))
readr::write_csv(result_df,
                 file.path(output_dir,
                          # "image_processing",
                          # "extended",
                           paste0("ResultTissueArea_",group_ID,".csv")))
