# version: 061021

#____________________________
#find_all_attributes_in_EDL()
#____________________________

## chunk function_find_all_attributes_in_EDL{

#' @title find_all_attributes_in_EDL()
#'
#' @description Reads a XML string, extracts all nodes, attributes and paths, reorganize the data and returns 3 different dataframes
#'
#' @details Die Funktion erzeugt zunächst aus dem EDL String ein XML formatiertes Objekt.
#' Von diesem XML Objekt werden alle Knoten ermittelt und für jeden Knoten ein XML-Nodeset, einschließlich der Dokumentwurzel, erstellt und als Spalte xml_node in den dataframe df eingefügt.
#' Ausgehend von den Nodesets wird für jeden Knoten der Name und der Pfad sowie alle Attribute ermittelt bzw. extrahiert und als Spalten node_name, node_path, node_attribute in df eingefügt.
#' Dabei ist im dataframe df die Spalte node_attributes eine genestete Spalte, die je nach Knoten eine unterschiedliche Anzahl an Attributen enthalten kann.
#' Die genestete Spalte node_attributes wird zwei reguläre Spalten node_attributes,node_attributes_id transformiert und mit den Spalten node_name,node_path,xml_node zum df-longer zusammengefasst.
#' Der df_wider enthält die gleichen Daten wie df_longer, mit dem Unterschied das für jeden Attributnamen eine Spalte eingefügt wurde, und die Attributwerte somit horizontal verteilt werden,  sodass die Zeilenanzahl der vom df entspricht.
#'
#' @param EDL :character containing a XML string
#'
#' @return a list of 3 dataframes df, df_longer, df_wider
#' @export find_all_attributes_in_EDL
#' @family database related
#'
#' @examples
#' if(FALSE){
#'
#' result_list <- find_all_attributes_in_EDL(EDL)
#' }
#'
find_all_attributes_in_EDL<-function(EDL){

  file <- try(EDL%>%
                xml2::read_xml(),
              silent=TRUE)

  if(inherits(file,'try-error')==FALSE){

    # create object containing all nodes in the EDL, including the root
    all_nodes <-file %>%
      xml2::xml_find_all(".//*")
    all_nodes<-c(list(file),purrr::map(all_nodes,~.x))
    print(paste0("EDL contains ",length(all_nodes)," nodes - including the EDL itself"))

    # create df containing a column with all nodes of the EDL
    df <-data.frame(xml_node=vector(length=length(all_nodes)))%>%
      dplyr::mutate(xml_node=purrr::map(all_nodes,~.x))

    #add name of node-object
    names(df$xml_node)<-purrr::map_chr(df$xml_node,~xml2::xml_name(.x))

    # extract node attributes and add them to df
    df <- df%>%
      dplyr::mutate(node_name=purrr::map_chr(xml_node,~.x%>%xml2::xml_name()),
                    node_attributes=purrr::map(xml_node,~.x%>%xml2::xml_attrs()),
                    node_path=purrr::map_chr(xml_node,~.x%>%xml2::xml_path()))

    # remove object numeration from path
    #df<-df%>%
    #  mutate(node_path=node_path %>% stringr::str_remove_all("\\[[0-9]+\\]"))

    # unnest the df, so that all attribute Values and Names are in a column
    df_longer <- df %>%
      tidyr::unnest_longer(node_attributes)%>%
      dplyr::select(node_name,node_attributes,node_attributes_id,node_path,xml_node)

    # unnest the df with attribute_id as columns
    df_wider<-df%>%
      tidyr::unnest_wider(node_attributes)

    return(attribute_dfs=list(df=df,df_longer=df_longer,df_wider=df_wider))

  }else(return("error_cant read input as XML"))
}
##}
