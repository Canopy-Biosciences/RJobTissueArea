V <- "260422"
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
    "- create_result_filepath()"
  ))

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

create_name_result_ID <- function(chip_ID,
                                  pos,
                                  sigma,
                                  threshold){

  result_ID <- paste0("chipID_",chip_ID,
                      "_pos_",pos,
                      "_sigma_",sigma,
                      "_threshold_",threshold)
}

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
