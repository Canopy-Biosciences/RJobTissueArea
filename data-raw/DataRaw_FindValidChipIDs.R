group_ID <- "P1761451"

query_groupID_limslager <- query_mongoDB("limslager","UID",group_ID)

EDL_groupID_limslager <- query_chipIDs_limslager%>%get_EDL_from_query_result()

chip_IDs <- find_valid_group_chip_IDs(group_ID)
# resulted the following chipIDs
#c("M1730408", "M1730410", "M1730412", "M1730416", "M1730420",
#  "M1730422", "M1730424", "M1730426", "M1708179", "M1708181", "M1730444",
#  "M1730446", "M1708167", "M1708169", "M1708175", "M1708177", "M1764682",
#  "M1730404", "M1730406", "M1764730")

query_chipIDs_limslager <- query_mongoDB("limslager","UID",chip_IDs)

EDL_chipIDs_limslager <- query_chipIDs_limslager%>%get_EDL_from_query_result()

server_path <- find_server_path()

usethis::use_data(group_ID,
                  chip_IDs,
                  overwrite = TRUE)


save(query_groupID_limslager,
     EDL_groupID_limslager,
     query_chipIDs_limslager,
     EDL_chipIDs_limslager,
     server_path,
     file="data-raw/SysDataRaw_FindValidChipIDs")
#load("data-raw/SysDataRaw_FindValidChipIDs")

