
renv::install("r-tensorflow/unet") #remotes::install_github("r-tensorflow/unet")
#miniconda is installed
#renv::install("tensorflow")
#new session
#tensorflow::install_tensorflow()

reticulate::install_python("3.8.7")

library(unet)

# takes additional parameters, including number of downsizing blocks,
# number of filters to start with, and number of classes to identify
# see ?unet for more info
model <- unet(input_shape = c(128, 128, 3))





# libraries we're going to need later
library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)

images <- tibble(
  img = list.files(here::here("inst/data/umap/train"), full.names = TRUE),
  mask = list.files(here::here("inst/data/umap/train_masks"), full.names = TRUE)
) %>%
  sample_n(2) %>%
  map(. %>% magick::image_read() %>% magick::image_resize("128x128"))

out <- magick::image_append(c(
  magick::image_append(images$img, stack = TRUE),
  magick::image_append(images$mask, stack = TRUE)
)
)






tensorflow::install_tensorflow()
renv::deactivate()
