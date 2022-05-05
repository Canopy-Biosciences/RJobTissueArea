V <- "030522"
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
    "- create_export_data_sum()"
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
  Version <- "030522"
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
