V <- 170622
targets <- "complete TissueAreaDetection"
assign(paste0("version.targets.", targets), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start tragets workflow Rsript- ", targets, ", Version ", V))
writeLines("")

library(targets)
#__________________
#set user input----
#groupID <-
#outputdir <-
#sigma <-
#threshold <-
#window <- c(10,30)
#RJobTissueArea::create_working_directory(output_dir)
#

tar_script({

  options(crayon.enabled = FALSE)
  tar_option_set(memory = "transient", garbage_collection = TRUE)
  tar_option_set(packages = c("RJobTissueArea","tibble", "readr", "dplyr", "ggplot2"),
                 imports = "RJobTissueArea")
  Sys.setenv(TAR_WARN = "false")
  tar_config_set(store = file.path("inst","analysis_workflow"),script = "target_compl.R")
  #load all package functions
  #Rfiles <- list.files("R")
  #purrr::walk(Rfiles,~source(file.path("R",.x)))


  mapping_imagelist_calculate_data_sum <- function(image_groups){
    purrr::map(image_groups,~calculate_data_sum(.x))

    }



  list(
    tar_target(group_ID, "P1761451"),
    tar_target(output_dir, "devel/data/data_output"),
    tar_target(grid, expand.grid(sigma=c(1,15),threshold=c(1,2,5))),
   tar_target(chip_IDs, data(chip_IDs)),#find_valid_group_chip_IDs(group_ID))#,
   tar_target(ScanHistory, create_ScanHistory_extended(chip_IDs,output_dir,group_ID)),
   tar_target(filename, create_result_filepath(output_dir,"extendedScanHistory",group_ID,"csv")),
   tar_target(ScanHistory2,data.table::fread(filename)),
   tar_target(image_groups, create_hdr_image_groups(ScanHistory)),
   tar_target(data_sum,mapping_imagelist_calculate_data_sum(image_groups))

  )



})
tar_make()

tar_visnetwork()
tar_watch()

# create list of hdr images grouped by position_ID
# subsetting image data listed in ScanHistory


# calculate data_sum for all image groups----



# apply tissue detection workflow----
#process_TissueDetection(image_groups,
#                        output_dir,
#                        sigma = 15,
#                        threshold = 2,
#                        window = 10)
#
#





