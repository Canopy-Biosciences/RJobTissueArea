library(magrittr)
#______________
#install pkgapi----
#package_dir <- file.path("devel/config/FunctionsPackageDependencyTools")
#package_name <- "pkgapi-main.zip"
#install.packages(file.path(package_dir,package_name), repos = NULL, type = "win.binary")
#_______________________________
#create functions dependency map----
pkg_name <- "RJobTissueArea"
map <- pkgapi::map_package()
internal_calls <- map$calls[map$calls$to %in% glue::glue("{map$name}::{map$defs$name}"),]
internal_calls$to <- stringr::str_remove(internal_calls$to,paste0(pkg_name,"::"))
#___________________
#create graph object----
gtbl <- internal_calls%>%
  tidygraph::as_tbl_graph()%>%
  tidygraph::as.igraph()
#___________________________
#get_all_network_funktions()----
get_all_network_functions <- function(main_function,
                                      gbtl){
  #get_network_functions("find_valid_group_chip_IDs")
  n <- helper <- NULL
  n <- neighbors(gtbl,main_function)%>%names()
  helper <- n
  while(length(n)>0){
    n <- purrr::map(n,~neighbors(gtbl,.x)%>%names())%>%unlist()
    helper <- c(helper, n)
  }
  return(helper%>%unique())
}
#____________________
#define mainfunctions----
main_functions <- c("create_hdr_image_groups",
                    "create_ScanHistory_extended",
                    #"create_working_directory",
                    #"export_list_all_image_files",
                    "find_valid_group_chip_IDs",
                    "process_TissueDetection")#,
                    #"process_tissue_detection_workflow")
#_______________________________
#get all helpers of each mainfkt----
all_functions <- purrr::map(main_functions,
                            ~data.frame(caller = get_network_functions(.x,gtbl)))%>%
                              data.table::rbindlist()
#_________________
#join helperfile.R----
all_functions <- all_functions%>%
  dplyr::left_join(map[["defs"]]%>%
                     dplyr::select(file,name),
                   by=c("caller"="name"))%>%
  dplyr::distinct()%>%
  dplyr::group_by(file)%>%
  tidyr::nest()
#___________________
#create new R folder----helperfile----
create_working_directory("newR/R")
#_______________________
#add helper-subset-files---
purrr::walk2(all_functions$file,
             all_functions$data,#
             ~dump(.y[[1]],
                   file = file.path("newR",.x),
                   envir = (as.environment("package:RJobTissueArea"))))
