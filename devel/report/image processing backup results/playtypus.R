remotes::install_github("maju116/platypus")
library(tidyverse)
library(platypus)
library(abind)

test_yolo <- yolo3(
  net_h = 416, # Input image height. Must be divisible by 32
  net_w = 416, # Input image width. Must be divisible by 32
  grayscale = FALSE, # Should images be loaded as grayscale or RGB
  n_class = 80, # Number of object classes (80 for COCO dataset)
  anchors = coco_anchors # Anchor boxes
)

test_yolo

test_yolo %>% load_darknet_weights("inst/data/platypus/yolov3.weights")
reticulate::py_install("pillow",env=tf)

test_img_paths <- list.files(system.file("extdata", "images", package = "platypus"), full.names = TRUE, pattern = "coco")
test_imgs <- test_img_paths %>%
  map(~ {
    image_load(., target_size = c(416, 416), grayscale = FALSE) %>%
      image_to_array() %>%
      `/`(255)
  }) %>%
  abind(along = 4) %>%
  aperm(c(4, 1:3))
test_preds <- test_yolo %>% predict(test_imgs)

str(test_preds)
#> List of 3
#>  $ : num [1:2, 1:13, 1:13, 1:3, 1:85] 0.294 0.478 0.371 1.459 0.421 ...
#>  $ : num [1:2, 1:26, 1:26, 1:3, 1:85] -0.214 1.093 -0.092 2.034 -0.286 ...
#>  $ : num [1:2, 1:52, 1:52, 1:3, 1:85] 0.242 -0.751 0.638 -2.419 -0.282 ...
