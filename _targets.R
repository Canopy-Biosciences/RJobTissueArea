library(targets)
options(crayon.enabled = FALSE)
tar_option_set(memory = "transient", garbage_collection = TRUE)
create_plot <- function(data) {
    ggplot(data) + geom_histogram(aes(x = Ozone), bins = 12) + 
        theme_gray(24)
}
list(tar_target(raw_data, airquality), tar_target(data, raw_data %>% 
    filter(!is.na(Ozone)) %>% dplyr::slice(1:20)), tar_target(hist, 
    data %>% create_plot()), tar_target(fit, biglm::biglm(Ozone ~ 
    Wind + Temp, data)))
