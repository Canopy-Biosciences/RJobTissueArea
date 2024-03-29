V <- "240822"
helpers <- "PlotScanResults"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- ()"
  ))



library(RJobTissueArea)
group_ID <- "stichings"
output_dir <- file.path("data_output",group_ID)
RJobTissueArea:::create_working_directory(output_dir)

#chip_IDs <- c("M1708167","M1708169","M1708181","M1730404","M1730406","M1730412","M1730424","M1730446","M1764730","M1730408","M1579140","M1579152","M1562406","M1562329","M1562341","M1562444","M1578587","M1708241")
chip_IDs <- c("M1579152","M1562406","M1730408","M1708169","M1562329","M1562341","M1562444","M1578587","M1730412")

#chip_ID <- c("M1562406")

ScanHistory <- create_ScanHistory_extended(chip_IDs,
                                           output_dir,
                                           result_ID = group_ID)

image_groups <- ScanHistory%>%
  dplyr::filter(Excluded %in% c("FALSE")) %>%
  dplyr::filter(Status == "Finished")%>%
  dplyr::filter(enabled == 1)%>%
  dplyr::filter(!is.na(hdr_filename)) %>%
  #dplyr::filter(!Tag %in% c("BG", "*")) %>%
  dplyr::mutate(hdr_filepath = create_hdr_filepath(chip_path, scan_ID, pos_ID))%>%
  dplyr::rename(image_path = "hdr_filepath",
                blob_filename = "hdr_filename") %>%
  dplyr::mutate(group_ID = paste0(chip_ID,"_", pos_ID))%>%
  dplyr::group_by(chip_ID, pos_ID) %>%
  tidyr::nest() %>%
  dplyr::mutate(group_ID = paste0(chip_ID,"_", pos_ID))

#_______
#dataSum----
dataSum <- purrr::map(image_groups$data[1:18],~calculate_data_sum(.x))

#________________________
#attenuate FL peak values----
data_attenuate <- purrr::map(dataSum,
                             ~attenuate_FLpeakValues(.x,
                                                     attenuation=0.01))

#____________
#reduce noise----
data_noi <- purrr::map(data_attenuate,
                       ~reduce_noise(.x))

#_________________
#convert to matrix----
m.data <- purrr::map(data_noi,
                     ~convert_image_vector_to_matrix(.x))

#__________________
#extract resolution----
cellres <- purrr::map(dataSum,
                      ~extract_image_resolution(.x))

#________________________________
#apply tissue detection procedure----
pixelsets <- purrr::map2(m.data,
                        cellres,
                         ~process_tissue_detection_workflow(.x,
                                                           .y,
                                                           sigma=15,
                                                           threshold=35,
                                                           window=50))

names(pixelsets)<-image_groups$pos_ID[1:18]





#___________________
#load positions file----
chip_paths <- purrr::map_chr(chip_IDs,
                             ~find_chip_path(.x))
positions_paths <- file.path(chip_paths,"positions.csv")
check_posFiles <- file.exists(positions_paths)
pos_files <- purrr::map(positions_paths[check_posFiles],
                        ~readr::read_delim(.x,delim="\t"))
names(pos_files)<- chip_IDs[check_posFiles]




#_____________________
#scale chipx and chipy----
chip_ID <- unique(image_groups$chip_ID[1:18]%>%unique())
positions <- pos_files[[chip_ID]]
sel <- which(positions$position %in% as.numeric(names(pixelsets)))
positions<-positions[sel,]

pixelset <- pixelsets[[1]]
i<- pixmap::pixmapGrey(pixelset)


#' sets high FL values to a certain quantile FL value of a given attenuation quantile
#'
#' attentuation defines the percentage of high FL values to be set to a lower cutoff value
#'
#' @param data_sum
#' @param attenuation
#'
#' @return
#' @export
#'
#' @examples
#' \donttest{
#'
#' group_ID <- "P1761451"
#' output_dir <- "data_output"
#' RJobTissueArea:::create_working_directory(output_dir)
#' chip_IDs <- find_valid_group_chip_IDs(group_ID)
#' ScanHistory <- create_ScanHistory_extended(chip_IDs,
#' output_dir,
#' result_ID = group_ID)
#' image_groups <- create_hdr_image_groups(ScanHistory)
#' image_group_list<-image_groups$data[[1]]
#' data_sum <- calculate_data_sum(image_group_list)
#' data_att <- attenuate_FLpeakValues(data_sum,0.1)
#' m.data <- convert_image_vector_to_matrix(data_att)
#' grey_values <- create_pixmap_greyvalues(m.data,c(1,1))
#' grey_values%>%
#' imager::as.cimg()%>%
#' plot(main = "original dataSum")
#' }
attenuate_FLpeakValues <- function(data_sum,attenuation=0.01){

  att <- att_threshold <- pos <- data_att <- NULL

  att <- 1-attenuation
  att_threshold <- quantile(data_sum,att)
  pos <- which(data_sum >= att_threshold)
  data_att <- data_sum
  data_att[pos] <- att_threshold

  return(data_att)

}

#' calculate_data_sum
#'
#' @param image_group_list
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
calculate_data_sum <- function(image_list){

  V <- 240522

  fileExist <- NULL
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
    Tag <- image_list$Tag[i]

    #_________________
    #read image binary
    #data_mat <- read_binary_image_as_matrix(image_path,
    #                                        blob_filename)

    if(file.exists(file.path(image_path,
                             blob_filename))){
      fileExist <- TRUE

    }else{
      fileExist <- FALSE
      writeLines(paste0("! file for ",Tag," in ",image_path," does not exist"))
      }

    if(fileExist){


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
  }
  return(data_sum)
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


#' create_hdr_image_groups
#'
#' @param ScanHistory
#'
#' @return
#' @export
#'
#' @examples
#'
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
#' @param attenuation
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
                                  attenuation,
                                  chip_ID,
                                  pos_ID,
                                  suffix_resultfile){
  #create output directory----
  plot_filepath <- file.path(
    output_dir,
    "image_processing",
    "result_plots",
    paste0(sigma,"_",threshold,"_",window,"_",attenuation)
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


#' process_TissueDetection
#'
#' @param image_groups
#' @param output_dir
#' @param sigma
#' @param threshold
#' @param window
#' @param attenuation
#'
#' @return
#' @export
#'
#' @examples
#' output_dir <- "devel/data/data_output"
#' result_ID <- "P1761451"
process_TissueDetection <- function(image_groups,
                                    output_dir,
                                    sigma = 15,
                                    threshold = 2,
                                    window = 10,
                                    attenuation = 0.01,
                                    plot_image = TRUE,
                                    result_ID = ""){
  V <- 220822
  # UPDATE
  # internal data_sum calculation
  # optional plot generation
  # TissueArea mm²
  # chip-wised summarized results
  # export and return result_df and summary_df
  # result_ID as label in result filenames
  # include check if result file exist and load it or create new result_df
  # include attenuation of FL peak values
  # include noise reduction

  #_______________________
  #create result_dirname----
  result_dir <- file.path(output_dir,
                          "image_processing")

  #______________________
  #create result_filename----
  result_filename <- file.path(result_dir,
                               paste0("ResultTissueArea_",
                                      result_ID,".csv"))
  #________________
  #create result_df----
  result_df <- tibble::tibble(
    group_ID = character(0), # group_ID
    chip_ID = character(0), #chip_ID
    pos_ID = numeric(0), #pos_ID
    attenuation = numeric(0),
    sigma = numeric(0),
    threshold = numeric(0),
    GS_window = numeric(0),
    perc_TissueArea = double(0),
    TissueArea_mm2 = double(0)
  )
  #_________________
  #add existing data----
  result_df<-read_result_file(result_df,result_filename)

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

    #________________________
    #attenuate FL peak values----
    data_attenuate <- attenuate_FLpeakValues(data_sum,
                                             attenuation)

    #____________
    #reduce noise----
    data_noi <- reduce_noise(data_attenuate)

    #_________________
    #convert to matrix----
    m.data <- convert_image_vector_to_matrix(data_noi)

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
                      attenuation = attenuation,
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
                                        attenuation,
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
    dplyr::group_by(chip_ID, attenuation,sigma, threshold, GS_window)%>%
    dplyr::summarize(n_positions = dplyr::n(),
                     perc_TissueArea = sum(perc_TissueArea)/dplyr::n(),
                     TissueArea_mm2 = sum(TissueArea_mm2),
                     .groups = "keep")

  #________________
  #export result_df----
  readr::write_excel_csv(result_df,
                   result_filename)

  #_____________________
  #export summary_result----
  summary_filename <- file.path(result_dir,
                               paste0("SummaryResultTissueArea_",
                                      result_ID,".csv"))

  readr::write_excel_csv(result_df_summary,
                   summary_filename)


  #_______________________________
  #return result_df and summary_df----
  return(list(result_df=result_df,
              summary_df = result_df_summary))

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


#' determines noise and sets FLvalues to zero
#'
#' - calculates density of FLvalues and determines all peaks
#' - defines the first density peak as noise
#' - noise threshold equals the last FLvalue of the first peak in the density curve
#' - sets all FLvalues below noise threshold to zero
#'
#' @param data vector or metrix of values
#'
#' @return vector of metric of values with noise values set to zero
#' @export
#' @keywords internal
#' @family
#'
#' @examples
#' \donttest{
#' group_ID <- "P1761451"
#' output_dir <- "data_output"
#' RJobTissueArea:::create_working_directory(output_dir)
#' chip_IDs <- find_valid_group_chip_IDs(group_ID)
#' ScanHistory <- create_ScanHistory_extended(chip_IDs,output_dir,group_ID)
#' image_groups <- create_hdr_image_groups(ScanHistory)
#' image_group_list<-image_groups$data[[51]]
#' data_sum <- calculate_data_sum(image_group_list)
#' data_attenuate <- attenuate_FLpeakValues(data_sum,0.01)
#' data_noi <- reduce_noise(data_attenuate)
#' m.data <- convert_image_vector_to_matrix(data_noi)
#' cellres <- extract_image_resolution(data_sum)
#' grey_values <- create_pixmap_greyvalues(m.data,cellres)
#' grey_values%>%
#' imager::as.cimg()%>%
#' plot(main = "original dataSum")
#' }
reduce_noise <- function(data){

  V <- 220822

  data_dens <- peaks <- pos_peak <- pos_noise <- noise <- data_noi <- NULL

  # estimate FLvalue density
  data_dens <- density(data)

  # detect peaks of density curve
  peaks <- pracma::findpeaks(data_dens$y)

  # get index of end value of the first density peak
  pos_peak <- min(peaks[,4])

  # get corresponding FLvalue
  noise <- data_dens$x[pos_peak]

  # determine all FLvalues less than noise
  pos_noise <- which(data <= noise)

  # set noise values to zero
  data_noi <- data
  data_noi[pos_noise] <- 0

  return(data_noi)

}
