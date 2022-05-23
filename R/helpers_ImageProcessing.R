V <- "230522"
helpers <- "ImageProcessing"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- perform_image_processing()",
    "- export_image_result_tiffs()",
    "- create_name_result_ID()",
    "- create_result_filepath()",
    "- create_export_data_sum()",
    "- create_hdr_image_groups()",
    "- calculate_data_sum()",
    "- process_data_sum_for_image_groups()",
    "- read_data_sum_image_resolution()",
    "- read_data_sum_as_matrix()",
    "- process_tissue_detection_workflow()",
    "- create_pixmap_greyvalues()",
    "- calculate_perc_TissueArea()",
    "- calculate_n_TissuePixel()",
    "- create_plot_directory()",
    "- process_TissueDetection()"
  ))


#' export_image_result_tiffs
#'
#' @param image
#' @param image_blurred
#' @param image_binary
#' @param output_dir
#' @param chip_ID
#' @param pos_ID
#' @param sigma
#' @param threshold
#' @param ncols
#' @param nrows
#'
#' @return
#' @export
#'
#' @examples
export_image_result_tiffs <- function(image,
                                      image_blurred,
                                      image_binary,
                                      output_dir,
                                      chip_ID,
                                      pos_ID,
                                      sigma,
                                      threshold,
                                      ncols,
                                      nrows){
  Version <- "030522"
  # check output.dir

  library(pixmap)
  create_working_directory(output_dir)

  # filename
  result_ID <- create_name_result_ID(chip_ID,
                                     pos_ID,
                                     sigma,
                                     threshold)
  # complete filepath
  file <- create_result_filepath(
    output_dir,
    "result_tiffs",
    result_ID,
    "tiff"
  )

  # create tiff object
  tiff(filename = file,
       width = ncols*3,
       height = nrows)

  par(mfrow=c(1,3))

  # plot inital image
  plot(image,
       main = "grey value image",
       sub = "sum values by pixels across all hdr images")

  # plot blurred image
  plot(image_blurred,
       main = paste0("blurred with sigma = ",sigma),
       sub = paste0("min: ", min(xb), "max: ",max(xb)))

  # plot binary image
  plot(image_binary,
       main= paste0("threshold set to: ",threshold))

  dev.off()

  writeLines(c(
    paste0("- successful exported processed image results of: ",result_ID)
  ))
}
#' create_name_result_ID
#'
#' @param chip_ID
#' @param pos
#' @param sigma
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
create_name_result_ID <- function(chip_ID,
                                  pos_ID,
                                  sigma,
                                  threshold){

  Version <- "030522"

  result_ID <- paste0("chipID_",chip_ID,
                      "_pos_",pos_ID,
                      "_sigma_",sigma,
                      "_threshold_",threshold)

  return(result_ID)
}

#' Title
#'
#' @param output_dir
#' @param name_string
#' @param result_ID
#' @param type
#'
#' @return
#' @export
#'
#' @examples
create_result_filepath <- function(output_dir,
                                   name_string,
                                   result_ID,
                                   type){

  path <- file.path(
    output_dir,
    paste0(name_string,
           "_",result_ID,
           ".",type))

  return(path)
}

#' create_export_data_sum
#'
#' @param image_group_list
#' @param group_ID
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples
#' if(FALSE){
#'
#' data_sum_resultion <- create_export_data_sum(image_groups$data[[1]],
#' image_groups$group_ID[1],
#' output_dir)
#'
#' }
create_export_data_sum <- function(image_group_list,
                                   group_ID,
                                   output_dir){
#image_group_list <- image_groups$data[1][[1]]
#group_ID <- image_groups$group_ID[1]
  create_working_directory(output_dir)

  for(i in 1:dim(image_group_list)[1]){

    image_path <- image_group_list$image_path[i]
    blob_filename <- image_group_list$blob_filename[i]

    data_mat <- read_binary_image_as_matrix(image_path,
                                            blob_filename)

    if(i == 1){
      data_sum <- data_mat
    }else{

      if(any(attr(data_mat,"image_resolution") != attr(data_sum,"image_resolution"))){
        writeLines(c(
          paste0("- image_resolution changed with scan_ID: ", image_group_list$scan_ID[i])
        ))}

      data_sum <- data_sum+data_mat

    }
  }

  result_filename <- create_result_filepath(output_dir,
                                            "data_sum",
                                            group_ID,
                                            type="csv")
  data.table::fwrite(data_sum%>%
                       data.table::data.table(),
                     result_filename)

  return(attr(data_sum,"image_resolution"))

}

#' perform_image_processing
#'
#' @param data
#' @param cellres
#' @param output_dir
#' @param chip_ID
#' @param pos
#' @param sigma
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
perform_image_processing <- function(m.data,
                                     cellres = cellres,
                                     output_dir = output_dir,
                                     chip_ID,
                                     pos_ID,
                                     sigma = sigma,
                                     threshold = threshold){
  Version <- "220522"
  #data <- data_sum


  # create pixmap
  # cellres: pixel resolution in horizontal and vertical direction
  image <-pixmap::pixmapGrey(m.data,
                             cellres=cellres)

  # get grey values
  grey_values <- image@grey * 255

  # Low-pass Gaussian filter
  #remotes::install_version("locfit",version="1.5.9.4")
  #BiocManager::install("EBImage",force=TRUE)
  #EBImage version: 4281
  xb <- EBImage::gblur(grey_values,
                       sigma)
  # round values
  xb <- round(xb,digits = 1)

  # create blurred pixmap image
  image_blurred <- pixmap::pixmapGrey(xb,
                                      cellres=cellres)

  #threshold filtering
  pos <- which(xb > threshold)
  xt <- xb
  xt[which(xb > threshold)] <- 1
  xt[which(xb <= threshold)] <- 0

  # pixmap object of binary image
  image_binary <- image
  image_binary@grey <- xt

  # create tiff of result images
  export_image_result_tiffs(image,
                            image_blurred,
                            image_binary,
                            output_dir,
                            chip_ID,
                            pos_ID,
                            sigma,
                            threshold,
                            ncols,
                            nrows)

}

#' create_hdr_image_groups
#'
#' @param ScanHistory
#'
#' @return
#' @export
#'
#' @examples
create_hdr_image_groups <- function(ScanHistory,
                                    input_dir_dataSum){
  #________________________
  #subset valid image files----
  result_files <- select_valid_image_files(result_files=ScanHistory,
                                           type = NULL)

  #______________________
  #subset hdr image files----
  hdr_files <- select_hdr_files(result_files)

  #___________________________
  #create list of image_groups----
  image_groups <- hdr_files%>%
    dplyr::rename("image_path"="hdr_filepath",
                  "blob_filename"="hdr_filename")%>%
    dplyr::group_by(chip_ID,pos_ID)%>%
    tidyr::nest()%>%
    dplyr::mutate(group_ID = paste0(chip_ID,"_",pos_ID))%>%
    dplyr::mutate(data_file =  file.path(input_dir_dataSum,
                                         paste0("dataSum_",chip_ID,"_",pos_ID,".rds")))

  return(image_groups)

}

#' calculate_data_sum
#'
#' @param image_group_list
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
calculate_data_sum <- function(image_list,
                               filepath){

 # j <- 1
 # image_group_list <- image_groups$data[[j]]
 # group_ID <-  image_groups$group_ID[j]
 # chip_ID <- image_groups$chip_ID[j]
 # pos_ID <- image_groups$pos_ID[j]

  for(i in 1:dim(image_list)[1]){

    #__________________________
    #select single image entity

    image_path <- image_list$image_path[i]
    blob_filename <- image_list$blob_filename[i]

    #_________________
    #read image binary
    #data_mat <- read_binary_image_as_matrix(image_path,
    #                                        blob_filename)
    data <- read_binary_image_as_vector(image_path,
                                        blob_filename)

    #________________
    #add pixel values
    if(i == 1){
      data_sum <- data
    }else{

      #if(any(attr(data,"image_resolution") != attr(data_sum,"image_resolution"))){
      #  writeLines(c(
      #    paste0("- image_resolution changed with scan_ID: ", image_group_list$scan_ID[i])
      #  ))}

      data_sum <- data_sum+data

    }
  }

  #_______________
  #export data_sum
  saveRDS(data_sum,
          filepath)
}

#' process_data_sum_for_image_groups
#'
#' @param image_groups
#'
#' @return
#' @export
#'
#' @examples
process_data_sum_for_image_groups <- function(image_groups){


  purrr::map2(image_groups$data,
              image_groups$data_file,
              ~calculate_data_sum(.x,.y))

}

#' read_data_sum_as_matrix
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
read_data_sum_as_matrix <- function(filepath){
  #________________
  #read in data_sum
  data_sum <- readRDS(filepath)

  #_________________________
  #create matrix of data_sum
  m_data_sum <-matrix(data_sum,
                      ncol=attr(data_sum,"h_pixel"),
                      nrow=attr(data_sum,"v_pixel"),
                      byrow=TRUE)
  return(m_data_sum)
}

#' read_data_sum_image_resolution
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
read_data_sum_image_resolution <- function(filepath){

  #________________
  #read in data_sum
  data_sum <- readRDS(filepath)

  #________________________
  #extract image resolution
  cellres <- as.vector(attr(data_sum,"image_resolution"))

  return(cellres)
}

#' create_pixmap_greyvalues
#'
#' @param m.data
#' @param cellres
#'
#' @return
#' @export
#'
#' @examples
create_pixmap_greyvalues <- function(m.data,
                                     cellres){
  #_____________
  #create pixmap----
  # cellres: pixel resolution in horizontal and vertical direction

  image <-pixmap::pixmapGrey(m.data,
                             cellres=cellres)

  #_______________
  #get grey values----
  grey_values <- image@grey * 255

  return(grey_values)
}

#' process_tissue_detection_workflow
#'
#' @param m.data
#' @param cellres
#' @param sigma
#' @param threshold
#' @param window
#'
#' @return
#' @export
#'
#' @examples
process_tissue_detection_workflow <- function(m.data,
                                              cellres,
                                              sigma,
                                              threshold,
                                              window){
  #create pixmap----
  # cellres: pixel resolution in horizontal and vertical direction

  image <-pixmap::pixmapGrey(m.data,
                             cellres=cellres)

  #_______________
  #get grey values----
  grey_values <- image@grey * 255

  #________________________
  #Low-pass Gaussian filter----
  #remotes::install_version("locfit",version="1.5.9.4")
  #BiocManager::install("EBImage",force=TRUE)
  #EBImage version: 4281
  xb <- EBImage::gblur(grey_values,
                       sigma)

  #____________
  #round values----
  xb <- round(xb,digits = 1)

  #___________________________
  #create blurred pixmap image----
  image_blurred <- pixmap::pixmapGrey(xb,
                                      cellres=cellres)

  #___________________
  #threshold filtering----
  pos <- which(xb > threshold)
  xt <- xb
  xt[which(xb > threshold)] <- 1
  xt[which(xb <= threshold)] <- 0

  #_____________________________
  #pixmap object of binary image----
  image_binary <- image
  image_binary@grey <- xt

  #_______________
  #adapting window----
  xta <- EBImage::thresh(xt, w=1,h=1)

  image_adaptedThreshold <- pixmap::pixmapGrey(xta,
                                               cellres=cellres)

  #________________________
  #convert to imager object----

  imager_pxset <- imager::as.cimg(image_binary@grey)

  #___________
  #grow binary----
  xg <- imager::grow(imager_pxset, window, window, window)


  #_____________
  #shrink binary----
  xs <-imager::shrink(xg,window, window, window)

  return(xs)

}

#' create_plot_directory
#'
#' @param output_dir
#' @param sigma
#' @param threshold
#' @param window
#' @param chip_ID
#' @param pos_ID
#'
#' @return
#' @export
#'
#' @examples
create_plot_directory <- function(output_dir,
                                 sigma,
                                 threshold,
                                 window,
                                 chip_ID,
                                 pos_ID){
  #create output directory----
  plot_filepath <- file.path(
    output_dir,
    "image_processing",
    "result_plots",
    paste0(sigma,"_",threshold,"_",window)
  )
  create_working_directory(plot_filepath)

  # complete filepath----
  plot_filename <- create_result_filepath(
    plot_filepath,
    "ResultImages_",
    paste0(chip_ID,"_",pos_ID),
    "png"
  )

  return(plot_filename)
}

plot_tissue_detection <- function(m.data,
                                  cellres,
                                  pixelset,
                                  filename){

  #___________________________
  png(filename = filename, width = 1400*2, height = 1400)
  par(mfrow=c(1,2))

  # original convert to imager object and plot
  grey_values <- create_pixmap_greyvalues(m.data,
                                          cellres)

  grey_values%>%
    imager::as.cimg()%>%
    plot(main = "original dataSum")

  perc_pixel <- calculate_perc_TissueArea(pixelset)

  # shrinked image
  pixelset %>%
    plot(main=paste0(perc_pixel," % tissueArea"))

  dev.off()

}

#' calculate_perc_TissueArea
#'
#' @param pixelset
#'
#' @return
#' @export
#'
#' @examples
calculate_perc_TissueArea <- function(pixelset){
  #count tissue pixel----
  n_tissue_pixel <- calculate_n_TissuePixel(pixelset)
  n_pixel <- pixelset%>%length()
  perc_pixel <- round(n_tissue_pixel/n_pixel*100,digits=1)

  return(perc_pixel)
}

#' calculate_n_TissuePixel
#'
#' @param pixelset
#'
#' @return
#' @export
#'
#' @examples
calculate_n_TissuePixel <- function(pixelset){
  n_tissue_pixel <- which(pixelset == 1)%>%length()
  return(n_tissue_pixel)
}

#' process_TissueDetection
#'
#' @param image_groups
#' @param output_dir
#' @param sigma
#' @param threshold
#' @param window
#'
#' @return
#' @export
#'
#' @examples
process_TissueDetection <- function(image_groups,
                                    output_dir,
                                    sigma = 15,
                                    threshold = 2,
                                    window = 10){

  #create result_df----
  result_df <- tibble::tibble(
    group_ID = character(0), # group_ID
    chip_ID = character(0), #chip_ID
    pos_ID = numeric(0), #pos_ID
    sigma = numeric(0),
    threshold = numeric(0),
    GS_window = numeric(0),
    perc_TissueArea = double(0),
    InputImageFile = character(0), # m_data_file
    OutputPlotFile = character(0)
  )

  #__________________________
  #1) map through image group----

  j=1
  for(j in 1:dim(image_groups)[1]){


    #____________________________________________
    #__select single entities of image_group list----
    image_group_list <- image_groups$data[[j]]
    group_ID <-  image_groups$group_ID[j]
    chip_ID <- image_groups$chip_ID[j]
    pos_ID <- image_groups$pos_ID[j]
    data_file <- image_groups$data_file[j]



    #__________________
    #__read in data_sum----
    m.data <- read_data_sum_as_matrix(data_file)


    #_____________________
    #read image resolution----
    cellres <- read_data_sum_image_resolution(data_file)


    #________________________________
    #apply tissue detection procedure----
    pixelset <- process_tissue_detection_workflow(m.data,
                                                  cellres,
                                                  sigma,
                                                  threshold,
                                                  window)

    #_______________________________
    #calculate percentage TissueArea----
    perc_pixel <- calculate_perc_TissueArea(pixelset)

    #_____________________
    #generate result plots----
    filename <- create_plot_directory(output_dir,
                                      sigma,
                                      threshold,
                                      window,
                                      chip_ID,
                                      pos_ID)

    plot_tissue_detection(m.data,
                          cellres,
                          pixelset,
                          filename)


    #________________________________
    #____add pixel count to result_df----

    result_df <- result_df %>%
      tibble::add_row(group_ID = group_ID,
                      chip_ID = chip_ID,
                      pos_ID = pos_ID,
                      sigma = sigma,
                      threshold = threshold,
                      GS_window = window,
                      perc_TissueArea = perc_pixel,
                      InputImageFile = m_data_file,
                      OutputPlotFile = filename
      )

  }

  #________________
  #export result_df----
  create_working_directory(file.path(output_dir,
                                     "image_processing"))
  result_filename <- file.path(output_dir,
                               "image_processing",
                               paste0("ResultTissueArea_",
                                      sigma,"_",threshold,"_",window,"_",
                                      group_ID,".csv"))


  readr::write_csv(result_df,
                   result_filename)



}
