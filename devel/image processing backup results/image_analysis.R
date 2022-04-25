library(pixmap)
library(magrittr)
library(image.LineSegmentDetector)
library(image.ContourDetector)
library(image.CannyEdges)

file <- file.path("inst/data","data_sum_M1730408_pos11.csv")
data.sum <- data.table::fread(file)
##plot(data.sum)
m.data_sum <- data.sum%>% as.matrix()
image <-pixmapGrey(m.data_sum)
plot(image)

x <- image@grey * 255

linesegments <- image_line_segment_detector(x)
linesegments
plot(image)
plot(linesegments, add = TRUE, col = "red")


contourlines  <- image_contour_detector(x, Q = 2)
contourlines
plot(image)
plot(contourlines, add = TRUE, col = "red")

edges <- image_canny_edge_detector(x)
edges
plot(edges)
