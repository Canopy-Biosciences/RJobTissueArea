# RJobTissueArea (development version)

# RJobTissueArea 0.0.0.16

* removed functions not called by the mainfunctions (which were exported)
# RJobTissueArea 0.0.0.14

* roxygen doc, package data
* input checkmate
* visible variable binding
* taget workflow
* tests
--> for find_valid_group_chip_IDs

# RJobTissueArea 0.0.0.13

* removed unused functions from help documentation
* updated vignettes HowToUse, HowToInstall
* updated Readme
* included pkg_sources
* helpers_DevelTools: dependencies

# RJobTissueArea 0.0.0.12

* USER template 

# RJobTissueArea 0.0.0.11

* RSCRIPT extended image processing 

# RJobTissueArea 0.0.0.10

* R SCRIPT looping over image groups and parameter grid
* additional processing steps ausprobiert

# RJobTissueArea 0.0.0.9
* find hdr files uddate approach via extended ScanHistory
* UPDATE report: import hdr data
* UPDATE helpers: importBlobFiles
* APPLICATION: for every chip_ID and pos combination of the chip_group:
    - export all image files 
    - calcutation and export of data_sum (only blob32 = hdr)
    - export of blob parameters (only blob32 = hdr)

# RJobTissueArea 0.0.0.8

* NEW functions in helpers ImportBlobFiles:
    - read_BLOB_parameter_from_XML()
    - read_image_binary_file()
* NEW report key functions definition
* NEW helpes DBrelated:
    - create_long_node_df_from_XML()

# RJobTissueArea 0.0.0.7

* NEW FUNCTIONS for finding all image files:
    - export_list_all_image_files()
    - select_valid_image_files()
* reports on tissue detection parametersettings
* reports on image processing
* reports on tissue detection

# RJobTissueArea 0.0.0.6

* NEW helperfunctions_ImportBlobFiles (first selection) added
* update FINAL report: find all image files (including blob, blob32, png which are existing)

# RJobTissueArea 0.0.0.5

* NEW report: find all Blob image files (hdr, FLimages, deltaTL, posRef, focus) of all chip group scanIDs 

# RJobTissueArea 0.0.0.4

* NEW HELPERFUNCTIONS R file: MetadataScanPosition

# RJobTissueArea 0.0.0.3

* NEW REPORT: investigations in scan and position metadata of chip group

# RJobTissueArea 0.0.0.2

* NEW FUNCTION: find_valid_group_chip_IDs(group_ID)

# RJobTissueArea 0.0.0.1

* Added helpers_GenerateInputData.R (from FDR project, new Version: 080322: bugfix dependencies)
