j=51
j<- 10
j<- 20
#select single image list----
image_group_list <- image_groups$data[[j]]
group_ID <-  image_groups$group_ID[j]
chip_ID <- image_groups$chip_ID[j]
pos_ID <- image_groups$pos_ID[j]

#__________________
#calculate data_sum----
data_sum <- calculate_data_sum(image_group_list)
m.data <- convert_image_vector_to_matrix(data_sum)
cellres <- extract_image_resolution(data_sum)
grey_values <- create_pixmap_greyvalues(m.data,
                                        cellres)
grey_values%>%
  imager::as.cimg()%>%
  plot(main = "original dataSum")


#________________________
#attenuate FL peak values----
attenuation <- 0.01
data_attenuate <- attenuate_FLpeakValues(data_sum,
                                         attenuation)
m.data <- convert_image_vector_to_matrix(data_attenuate)
grey_values <- create_pixmap_greyvalues(m.data,
                                        cellres)
grey_values%>%
  imager::as.cimg()%>%
  plot(main = "FL peaks attenuate")

#____________
#reduce noise----


#reduce_noise <- function(data,noise=0.1){

#  noise_threshold <- pos <- data_noi <- NULL

  data_dens <- density(data_attenuate)
  peaks <- pracma::findpeaks(data_dens$y)
  pos_noise <- min(peaks[,4])
  noise <- data_dens$x[pos_noise]


  hist(data_attenuate)
  abline(v=data_dens$x[peaks[,4]],col="blue")
  abline(v=data_dens$x[peaks[,3]],col="green")
  abline(v=data_dens$x[peaks[,2]],col="red")

  pos <- which(data_attenuate <= noise)
  data_noi <- data_attenuate
  data_noi[pos] <- 0


m.data <- convert_image_vector_to_matrix(data_noi)
grey_values <- create_pixmap_greyvalues(m.data,
                                        cellres)
grey_values%>%
  imager::as.cimg()%>%
  plot(main = "noise removed")

#blurring----

image <-pixmap::pixmapGrey(m.data,
                           cellres=cellres)
grey_values <- image@grey * 255
xb <- EBImage::gblur(grey_values,
                     sigma)
xb <- round(xb,digits = 1)
image_blurred <- pixmap::pixmapGrey(xb,
                                    cellres=cellres)
image_blurred@grey%>%imager::as.cimg()%>%
  plot(main = "blurred")


#___________________
#threshold filtering----
threshold <- 10
pos <- which(xb > threshold)
xt <- xb
xt[which(xb > threshold)] <- 1
xt[which(xb <= threshold)] <- 0

image_binary <- image
image_binary@grey <- xt
image_binary@grey%>%imager::as.cimg()%>%
  plot(main = "blurred")

#_______________
#adapting window----
window <- 1
xta <- EBImage::thresh(xt, w=1,h=1)

image_adaptedThreshold <- pixmap::pixmapGrey(xta,
                                             cellres=cellres)
imager_pxset <- imager::as.cimg(image_binary@grey)

xg <- imager::grow(imager_pxset, window, window, window)
xg%>%imager::as.cimg()%>%
  plot(main = "growed")

xs <-imager::shrink(xg,window, window, window)
xs%>%imager::as.cimg()%>%
  plot(main = "shrink")

#__________________
# plot final result-----
grey_values <- create_pixmap_greyvalues(xs,
                                        cellres)

grey_values%>%
  imager::as.cimg()%>%
  plot(main = "final result")

