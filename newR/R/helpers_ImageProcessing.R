create_result_filepath <-
function (output_dir, name_string, result_ID, type) 
{
    path <- file.path(output_dir, paste0(name_string, "_", result_ID, 
        ".", type))
    return(path)
}
calculate_data_sum <-
function (image_list) 
{
    V <- 240522
    for (i in 1:dim(image_list)[1]) {
        image_path <- image_list$image_path[i]
        blob_filename <- image_list$blob_filename[i]
        data <- read_binary_image_as_vector(image_path, blob_filename)
        if (i == 1) {
            data_sum <- data
        }
        else {
            data_sum <- data_sum + data
        }
    }
    return(data_sum)
}
calculate_perc_TissueArea <-
function (pixelset) 
{
    n_tissue_pixel <- calculate_n_TissuePixel(pixelset)
    n_pixel <- pixelset %>% length()
    perc_pixel <- round(n_tissue_pixel/n_pixel * 100, digits = 1)
    return(perc_pixel)
}
create_plot_directory <-
function (output_dir, sigma, threshold, window, chip_ID, pos_ID, 
    suffix_resultfile) 
{
    plot_filepath <- file.path(output_dir, "image_processing", 
        "result_plots", paste0(sigma, "_", threshold, "_", window))
    create_working_directory(plot_filepath)
    plot_filename <- create_result_filepath(plot_filepath, "ResultImages_", 
        paste0(chip_ID, "_", pos_ID, "_", suffix_resultfile), 
        "png")
    return(plot_filename)
}
plot_tissue_detection <-
function (m.data, cellres, pixelset, filename) 
{
    png(filename = filename, width = 1400 * 2, height = 1400)
    par(mfrow = c(1, 2))
    grey_values <- create_pixmap_greyvalues(m.data, cellres)
    grey_values %>% imager::as.cimg() %>% plot(main = "original dataSum")
    perc_pixel <- calculate_perc_TissueArea(pixelset)
    pixelset %>% plot(main = paste0(perc_pixel, " % tissueArea"))
    dev.off()
}
calculate_n_TissuePixel <-
function (pixelset) 
{
    n_tissue_pixel <- which(pixelset == 1) %>% length()
    return(n_tissue_pixel)
}
convert_image_vector_to_matrix <-
function (data_sum) 
{
    m_data_sum <- matrix(data_sum, ncol = attr(data_sum, "h_pixel"), 
        nrow = attr(data_sum, "v_pixel"), byrow = TRUE)
    return(m_data_sum)
}
extract_image_resolution <-
function (image_data) 
{
    V <- 240522
    cellres <- as.vector(attr(image_data, "image_resolution"))
    return(cellres)
}
process_tissue_detection_workflow <-
function (m.data, cellres, sigma, threshold, window) 
{
    image <- pixmap::pixmapGrey(m.data, cellres = cellres)
    grey_values <- image@grey * 255
    xb <- EBImage::gblur(grey_values, sigma)
    xb <- round(xb, digits = 1)
    image_blurred <- pixmap::pixmapGrey(xb, cellres = cellres)
    pos <- which(xb > threshold)
    xt <- xb
    xt[which(xb > threshold)] <- 1
    xt[which(xb <= threshold)] <- 0
    image_binary <- image
    image_binary@grey <- xt
    xta <- EBImage::thresh(xt, w = 1, h = 1)
    image_adaptedThreshold <- pixmap::pixmapGrey(xta, cellres = cellres)
    imager_pxset <- imager::as.cimg(image_binary@grey)
    xg <- imager::grow(imager_pxset, window, window, window)
    xs <- imager::shrink(xg, window, window, window)
    return(xs)
}
calculate_TissueArea <-
function (n_pixel, cellres) 
{
    area <- n_pixel * cellres[1]/1000 * cellres[2]/1000
    return(area)
}
create_pixmap_greyvalues <-
function (m.data, cellres) 
{
    image <- pixmap::pixmapGrey(m.data, cellres = cellres)
    grey_values <- image@grey * 255
    return(grey_values)
}
