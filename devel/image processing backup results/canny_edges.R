library(image.CannyEdges)
if(requireNamespace("pixmap") && requireNamespace("magick")){
library(pixmap)
imagelocation <- system.file("extdata", "chairs.pgm", package="image.CannyEdges")
image <- read.pnm(file = imagelocation, cellres = 1)
x <- image@grey * 255
edges <- image_canny_edge_detector(x)
edges
plot(edges)
##
## image_canny_edge_detector expects a matrix as input
##  if you have a jpg/png/... convert it to pgm first or take the r/g/b channel
library(magick)
x <- image_read(system.file("extdata", "atomium.jpg", package="image.CannyEdges"))
x
image <- image_data(x, channels = "Gray")
image <- as.integer(image, transpose = TRUE)
edges <- image_canny_edge_detector(image)
plot(edges)
f <- tempfile(fileext = ".pgm")
library(magick)
x <- image_read(system.file("extdata", "atomium.jpg", package="image.CannyEdges"))
x <- image_convert(x, format = "pgm", depth = 8)
image_write(x, path = f, format = "pgm")
image <- read.pnm(f, cellres = 1)
edges <- image_canny_edge_detector(image@grey * 255)
plot(edges)
file.remove(f)
}
