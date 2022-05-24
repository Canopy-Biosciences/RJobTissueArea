V <- "240522"
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
    "- process_TissueDetection()",
    "- calculate_TissueArea()"
  ))





#' Title
#'
#' @param output_dir
#' @param name_string
#' @param result_ID
#' @param type
#'
#' @return
#' @export
#' @keywords internal
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


#' create_hdr_image_groups
#'
#' @param ScanHistory
#'
#' @return
#' @export
#'
#' @examples
create_hdr_image_groups <- function(ScanHistory){

  V <- 240522
  # UPDATE
  # no data_file column
  # no input_dir_dataSum

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
    #dplyr::mutate(hdr_file = file.path(hdr_filepath,hdr_filename))%>%
    dplyr::rename("image_path"="hdr_filepath",
                  "blob_filename"="hdr_filename")%>%
    dplyr::group_by(chip_ID,pos_ID)%>%
    tidyr::nest()%>%
    dplyr::mutate(group_ID = paste0(chip_ID,"_",pos_ID))

  return(image_groups)

}

#' calculate_data_sum
#'
#' @param image_group_list
#' @param filepath
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
calculate_data_sum <- function(image_list){

  V <- 240522
  # update
  #- only return data.sum

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
  return(data_sum)
}

#' export_data_sum
#'
#' @param image_groups
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
export_data_sum <- function(image_groups,
                            datasum_dir){


  V <- 2405022
  # UPDATE
  #- calculate_data_sum returns data_sum
  #- saveRDS
  #- renamed

  create_working_directory(datasum_dir)

  purrr::walk2(image_groups$data,
               image_groups$data_file,
               ~.x%>%
                 calculate_data_sum()%>%
                 saveRDS(.y))
}

#' Title
#'
#' @param data_sum
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
convert_image_vector_to_matrix <- function(data_sum){

  #_________________________
  #create matrix of data_sum
  m_data_sum <-matrix(data_sum,
                      ncol=attr(data_sum,"h_pixel"),
                      nrow=attr(data_sum,"v_pixel"),
                      byrow=TRUE)

  return(m_data_sum)

}

#' read_data_sum_as_matrix
#'
#' @param filepath
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
read_data_sum_as_matrix <- function(filepath){
  V <- 240522
  # UPDATE
  # matrix of data_sum by external function

  #________________
  #read in data_sum
  data_sum <- readRDS(filepath)

  #_________________________
  #create matrix of data_sum
  m_data_sum <- convert_image_vector_to_matrix(data_sum)

  return(m_data_sum)
}

#' reads the resolution attribute of an image object
#'
#' @param filepath
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
extract_image_resolution <- function(image_data){

  V <- 240522
  # UPDATE
  #- renamed
  #- extraction of the attribute only
  #- image data as input instead of filepath

  #________________________
  #extract image resolution
  cellres <- as.vector(attr(image_data,
                            "image_resolution"))

  return(cellres)
}

#' create_pixmap_greyvalues
#'
#' @param m.data
#' @param cellres
#'
#' @return
#' @export
#' @keywords internal
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
#' @keywords internal
#'
#' @examples
create_plot_directory <- function(output_dir,
                                 sigma,
                                 threshold,
                                 window,
                                 chip_ID,
                                 pos_ID,
                                 suffix_resultfile){
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
    paste0(chip_ID,"_",pos_ID,"_",suffix_resultfile),
    "png"
  )

  return(plot_filename)
}

#' Title
#'
#' @param m.data
#' @param cellres
#' @param pixelset
#' @param filename
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
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
#' @keywords internal
#'
#' @examples
calculate_perc_TissueArea <- function(pixelset){
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
#' @keywords internal
#'
#' @examples
calculate_n_TissuePixel <- function(pixelset){
  n_tissue_pixel <- which(pixelset == 1)%>%length()
  return(n_tissue_pixel)
}


#' calculate_TissueArea
#'
#' @param n_pixel
#' @param cellres
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
calculate_TissueArea <- function(n_pixel,cellres){
  area <- n_pixel*cellres[1]/1000*cellres[2]/1000
  return(area)
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
                                    window = 10,
                                    plot_image = TRUE,
                                    result_ID = ""){
  V <- 240522
  # UPDATE
  # internal data_sum calculation
  # optional plot generation
  # TissueArea mm²
  # chip-wised summarized results
  # export and return result_df and summary_df
  # result_ID as label in result filenames

  #________________
  #create result_df----
  result_df <- tibble::tibble(
    group_ID = character(0), # group_ID
    chip_ID = character(0), #chip_ID
    pos_ID = numeric(0), #pos_ID
    sigma = numeric(0),
    threshold = numeric(0),
    GS_window = numeric(0),
    perc_TissueArea = double(0),
    TissueArea_mm2 = double(0)
  )

  #________________________
  #map through image groups----

  j=1
  for(j in 1:dim(image_groups)[1]){

    #________________________
    #select single image list----
    image_group_list <- image_groups$data[[j]]
    group_ID <-  image_groups$group_ID[j]
    chip_ID <- image_groups$chip_ID[j]
    pos_ID <- image_groups$pos_ID[j]

    #__________________
    #calculate data_sum----
    data_sum <- calculate_data_sum(image_group_list)

    #_________________
    #convert to matrix----
    m.data <- convert_image_vector_to_matrix(data_sum)

    #__________________
    #extract resolution----
    cellres <- extract_image_resolution(data_sum)

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
    # calculate TissueArea----
    TissueArea <- pixelset%>%
      calculate_n_TissuePixel()%>%
      calculate_TissueArea(cellres)

    #__________________
    #complete result_df----
    result_df <- result_df %>%
      tibble::add_row(group_ID = group_ID,
                      chip_ID = chip_ID,
                      pos_ID = pos_ID,
                      sigma = sigma,
                      threshold = threshold,
                      GS_window = window,
                      perc_TissueArea = perc_pixel,
                      TissueArea_mm2 = TissueArea
      )

    #________________________________
    #generate result plots (optional)----
    if(plot_image){
      filename <- create_plot_directory(output_dir,
                                        sigma,
                                        threshold,
                                        window,
                                        chip_ID,
                                        pos_ID,
                                        result_ID)

      plot_tissue_detection(m.data,
                            cellres,
                            pixelset,
                            filename)

    }
  }

  #________________
  #chipwise summary----
  result_df_summary <- result_df%>%
    dplyr::group_by(chip_ID, sigma, threshold, GS_window)%>%
    dplyr::summarize(n_positions = dplyr::n(),
                     perc_TissueArea = sum(perc_TissueArea)/dplyr::n(),
                     TissueArea_mm2 = sum(TissueArea_mm2),
                     .groups = "keep")


  #_______________________
  #create result directory----
  result_dir <- file.path(output_dir,
                          "image_processing")
  create_working_directory(result_dir)

  #________________
  #export result_df----
  result_filename <- file.path(result_dir,
                               paste0("ResultTissueArea_",
                                      result_ID,".csv"))

  readr::write_csv(result_df,
                   result_filename)

  #________________
  #export result_df----
  summary_filename <- file.path(result_dir,
                               paste0("SummaryResultTissueArea_",
                                      result_ID,".csv"))

  readr::write_csv(result_df_summary,
                   summary_filename)


  return(list(result_df=result_df,
              summary_df = result_df_summary))

}
