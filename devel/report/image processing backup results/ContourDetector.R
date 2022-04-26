library(pixmap)
library(image.ContourDetector)
imagelocation <- system.file("extdata", "image.pgm", package="image.ContourDetector")
image         <- read.pnm(file = imagelocation, cellres = 1)

x             <- image@grey * 255
contourlines  <- image_contour_detector(x, Q = 2)
contourlines
plot(image)
plot(contourlines, add = TRUE, col = "red")


##
## line_segment_detector expects a matrix as input
##  if you have a jpg/png/... convert it to pgm first or take the r/g/b channel

library(magick)
x   <- image_read(system.file("extdata", "atomium.jpg", package="image.ContourDetector"))
x
mat <- image_data(x, channels = "gray")
mat <- as.integer(mat, transpose = TRUE)
mat <- drop(mat)
contourlines <- image_contour_detector(mat)
plot(contourlines)


##
##  working with a RasterLayer
##


library(raster)
x   <- raster(system.file("extdata", "landscape.tif", package="image.ContourDetector"))

contourlines <- image_contour_detector(x)
image(x)
plot(contourlines, add = TRUE, col = "blue", lwd = 10)


