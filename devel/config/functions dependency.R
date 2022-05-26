library(pkgapi)

map <- pkgapi::map_package()

str(map$defs)

str(map$calls)

library("magrittr")
internal_calls <- map$calls[map$calls$to %in% glue::glue("{map$name}::{map$defs$name}"),]

internal_calls %>%
  dplyr::arrange(to)


library("visNetwork")
internal_calls <- internal_calls %>%
  dplyr::mutate(to = gsub("cranlogs\\:\\:", "", to))

nodes <- tibble::tibble(id = map$defs$name,
                        title = map$defs$file,
                        label = map$defs$name,
                        shape = dplyr::if_else(map$defs$exported,
                                               "triangle",
                                               "square"))

edges <- internal_calls[, c("from", "to")]


visNetwork(nodes, edges, height = "500px") %>%
  visLayout(randomSeed = 42) %>%
  visNodes(size = 10)
