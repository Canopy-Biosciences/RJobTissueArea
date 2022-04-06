library(pixmap)
library(image.LineSegmentDetector)
imagelocation <- system.file("extdata", "chairs.pgm", package="image.LineSegmentDetector")
image <- read.pnm(file = imagelocation, cellres = 1)
x <- image@grey * 255

linesegments <- image_line_segment_detector(x)
linesegments
plot(image)
plot(linesegments, add = TRUE, col = "red")


imagelocation <- system.file("extdata", "le-piree.pgm", package="image.LineSegmentDetector")
image <- read.pnm(file = imagelocation, cellres = 1)
linesegments <- image_line_segment_detector(image@grey * 255)
plot(image)
plot(linesegments)


##
## image_line_segment_detector expects a matrix as input
##  if you have a jpg/png/... convert it to pgm first or take the r/g/b channel
library(magick)
x   <- image_read(system.file("extdata", "atomium.jpg", package="image.LineSegmentDetector"))
mat <- image_data(x, channels = "gray")
mat <- as.integer(mat, transpose = TRUE)
mat <- drop(mat)
linesegments <- image_line_segment_detector(mat)
plot(linesegments, lwd = 2)
