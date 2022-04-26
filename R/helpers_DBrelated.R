V <- "260422"
helpers <- "DBrelated"

assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c(
    "---------------------",
    "functions: ",
    "- create_long_node_df_from_XML()"
    ))

create_long_node_df_from_XML <- function(XML){

  all_nodes <- XML %>%
    xml2::xml_find_all(".//*")

  all_nodes <- purrr::map(all_nodes,
                          ~.x)

    df <- data.frame(xml_node = vector(length = length(all_nodes))) %>%
    dplyr::mutate(xml_node = purrr::map(all_nodes, ~.x))

    names(df$xml_node) <- purrr::map_chr(df$xml_node, ~xml2::xml_name(.x))

  df <- df %>% dplyr::mutate(node_name = purrr::map_chr(xml_node,
                                                        ~.x %>% xml2::xml_name()), node_attributes = purrr::map(xml_node,
                                                                                                                ~.x %>% xml2::xml_attrs()), node_path = purrr::map_chr(xml_node,
                                                                                                                                                                       ~.x %>% xml2::xml_path()))
  df_longer <- df %>% tidyr::unnest_longer(node_attributes) %>%
    dplyr::select(node_name, node_attributes, node_attributes_id,
                  node_path, xml_node)%>%
    dplyr::select(-xml_node)%>%
    as.data.frame()

  return(df_longer)
}

