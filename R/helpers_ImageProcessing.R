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

#' Title
#'
#' @param data_sum
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
perform_image_processing <- function(data_sum,
                                     cellres = cellres,
                                     output_dir = output_dir,
                                     chip_ID,
                                     pos,
                                     sigma = sigma,
                                     threshold = threshold){

  # create data.sum

  # export data.sum

  # define data_sum as matrix
  m.data_sum <- data.sum%>%
    as.matrix()

  # create pixmap
  # cellres: pixel resolution in horizontal and vertical direction
  image <-pixmap::pixmapGrey(m.data_sum,
                             nrow = rows,
                             ncol = cols,
                             cellres=cellres)

  # get grey values
  grey_values <- image@grey * 255

  # Low-pass Gaussian filter
  xb <- EBImage::gblur(grey_values,
                       sigma)
  # round values
  xb <- round(xb,digits = 1)

  # create blurred pixmap image
  image_blurred <- pixmapGrey(xb,
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
                            pos,
                            sigma,
                            threshold,
                            cols,
                            rows)

}

#' Title
#'
#' @param image
#' @param image_blurred
#' @param image_binary
#' @param output_dir
#' @param chip_ID
#' @param pos
#' @param sigma
#' @param threshold
#' @param cols
#' @param rows
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
                                      pos,
                                      sigma,
                                      threshold,
                                      cols,
                                      rows){
  # filename
  result_ID <- create_name_result_ID(chip_ID,
                                     pos,
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
       width = cols*3,
       height = rows)

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
    paste0("- successful exported processed image results")
  ))
}

#' Title
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
                                  pos,
                                  sigma,
                                  threshold){

  result_ID <- paste0("chipID_",chip_ID,
                      "_pos_",pos,
                      "_sigma_",sigma,
                      "_threshold_",threshold)
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
create_export_data_sum <- function(image_group_list,
                                   group_ID,
                                   output_dir){

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
  data_sum <- data.table::data.table(data_sum)
  data.table::fwrite(data_sum,
                     result_filename)

  return(attr(data_sum,"image_resolution"))

}
