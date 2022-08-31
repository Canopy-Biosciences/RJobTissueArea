library(RJobTissueArea)
group_ID <- "FLpeakAttenuation_AND_noiseReduction"
output_dir <- file.path("data_output",group_ID)
RJobTissueArea:::create_working_directory(output_dir)

chip_IDs <- c("M1708167","M1708169","M1708181","M1730404","M1730406","M1730412","M1730424","M1730446","M1764730","M1730408","M1579140","M1579152","M1562406","M1562329","M1562341","M1562444","M1578587","M1708241")

chip_IDs <- c("M1579152","M1562406","M1730408","M1708169","M1562329","M1562341","M1562444","M1578587","M1730412")
