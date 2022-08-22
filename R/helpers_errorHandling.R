V <- "180622"
helpers <- "errorHandling"
#allgemeine tools
assign(paste0("version.helpers.", helpers), V)
writeLines("_____________________________________________________________")
writeLines(paste0("Start loading helper functions - ", helpers, ", Version ", V))
writeLines("")

writeLines(
  c("------------------------------",
    paste0("functions family: ",helpers),
    "------------------------------",
    "- check_EDLs_string()",
    "- check_mappingResultList()",
    "- detect_error_inCharacterVector()",
    "- get_errorDF_queryResults()",
    "- get_nonError_index()",
    "- handle_trychache_error()",
    "- is_error_in_EDLs()",
    "- is_error_in_mappingResult()",
    "- split_errorText_toDF()",
    "- print_queryResult_error()"
  ))

#____________________
#check_EDLs_strings()
#____________________

#' checks if EDL's contain readable XML-strings
#'
#' replaces EDLs by error messages if not a character or not readable
#'
#' @param EDLs character vector of XML-strings
#' @param IDs optional character vector of length(EDLs), containing IDs
#' @param print logical, error text should be printed
#'
#' @return EDLs character vector completed with error messages if neccessary
#' @export
#'
#' @keywords internal
#' @family errorHandling
#'
#' @examples
#' testEDL <- c("<foo><bar /></foo>",
#' "<ObjRef
#' UID=\"D1891203\"
#' Type=\"Gate\"/><ObjRef
#' UID=\"D1614318\"
#' Type=\"Gate\"/><ObjRef
#' UID=\"D973219\"
#' Type=\"Gate\"/>",
#' "lalala",NA,NULL)
#'
#' testEDL%>%check_EDLs_strings()
#'
#' testEDL%>%check_EDLs_strings(IDs = c("ID1","ID2","ID3","ID4"))
#'
#' testEDL%>%check_EDLs_strings(IDs = c("ID1"))
#'
check_EDLs_strings <- function(EDLs,IDs = NULL,print=FALSE){

  EDL_missing <- NULL

  if(is.null(IDs)==FALSE & length(EDLs)!=length(IDs)){
    IDs <- names(EDLs)
  }

  isCharacter <- purrr::map_lgl(EDLs,~ checkmate::test_character(.x))

  if(any(isCharacter == FALSE)){

    notCharacter <- which(isCharacter == FALSE)

    EDLs[notCharacter] <- paste0("error_EDL is not a character_",IDs[notCharacter])
    }


  if(length(which(isCharacter))>0){

   # isXML <- purrr::map_lgl(EDLs[isCharacter],
   #                               ~XML::isXMLString(.x))
    notXML2 <- purrr::map_lgl(EDLs,
                              ~inherits(try(.x%>%xml2::read_xml(),silent=TRUE),'try-error'))

   # if(any(isXML == FALSE)){
    if(any(notXML2)){
      #notXML <- which(isXML == FALSE)
      isXML2 <- which(notXML2 == FALSE)
      if(is.null(IDs)){
      #EDLs[notXML] <- paste0("error_EDL is not XML readable",names(EDLs)[notXML])
        EDLs[notXML2] <- paste0("error_EDL is not XML readable",names(EDLs)[notXML2])
      }else{
       # EDLs[notXML] <- paste0("error_EDL is not XML readable_",IDs[notXML])
        EDLs[notXML2] <- paste0("error_EDL is not XML readable_",IDs[notXML2])
      }

      if(print){
        #writeLines(paste0("!!! ",notXML%>%length()," EDL don't contain XML-string"))
        writeLines(paste0("!!! ",notXML2%>%length()," EDL can't be read with xml2 library"))
      }
      }
    return(EDLs)
  }
}

#__________________________
#check_mappingResultLists()
#__________________________

#' checks resultList and adds try-error text
#'
#' takes a result list and check if each element contains a try-error output text
#' replaces try-error output by an error text including ID of the element and a optional note
#'
#' @param result result list gained withing \code{~try(...,silent=TRUE)}
#' @param IDs character vector of IDs
#' @param error_text character of a suffix to add
#'
#' @return resultList with errorText
#' @export
#'
#' @keywords internal
#' @family errorHandling
#'
#' @examples
#' \donttest{
#'
#' chip_ID <- c("M911981","test","M911981")
#'
#' query_result <- query_UID_limslager(chip_ID)
#' query_error <- query_result$error_message
#'
#' EDLs <- get_EDLs_of_queryResult(query_result)
#' EDLs_error <- is_error_in_EDLs(EDLs)
#'
#' nodeset <- get_nodesets_of_EDLs(EDLs,chip_ID)
#' nodeset_error <- nodeset$error
#'
#' MH <- extract_MethodHistory(nodeset)
#' MH <-  MH%>%check_mappingResultLists()
#' MH_error <- MH%>%is_error_in_mappingResult()
#'
#' }
check_mappingResultLists <- function(result,
                                     IDs = NULL,
                                     error_text="no_MethodHistory in EDLchannel"){

  if(any(is.null(IDs), length(result)!=length(IDs))){
    IDs <- names(result)
  }

  isTryError <- purrr::map_lgl(result,
                               ~inherits(.x,'try-error'))

  if(any(isTryError)){
    result[isTryError] <- paste0("error_",error_text,"_",IDs[isTryError])
  }

  #purrr::map2(result,
  #                      IDs,
  #                      ~if(inherits(.x,'try-error')){
  #                        paste0("error_",error_text,"_",.y)
  #                      }else{.x})
  #

  #isDF <- purrr::map_lgl(result,~.x%>%checkmate::testDataFrame())

  return(result)

}

#________________________________
#detect_error_inCharacterVector()
#________________________________

#' detects given string in character vector
#'
#' @param error character vector
#' @param error_string character containing the string to detect
#'
#' @return logical if error text could be detected
#' @export
#'
#' @keywords internal
#' @family errorHandling
#'
#' @examples
#'
#' error <- c(try(ln(0),silent = TRUE), "error_", NA,"<xml>")
#' detect_error_inCharacterVector(error)
#'
detect_error_inCharacterVector <- function(error,
                                           error_string= c("error","Error")){

  isError <- NULL

  isError <- purrr::map_lgl(error,
                            ~stringr::str_detect(.x,error_string)%>%
                              any())

  return(isError)
}

#__________________________
#get_errorDF_queryResults()
#__________________________

#' checks mongoDB query result for missing entities
#'
#' returns a dataframe with error_IDs and error_text generated by the query_mongoDB().
#'
#' @param query_result list, generated by query_mongoDB, containing a list result and error_message
#' @param error_string character of a string defining an error
#'
#' @return dataframe of error_ID and error_text
#' @export
#'
#' @keywords internal
#' @family errorHandling
#'
#' @examples
#' \donttest{
#' query_result <- c("P1761451","re","t")%>%query_UID_limslager()
#' error_df <- query_result%>%get_errorDF_queryResults()
#' error_df <- query_result%>%get_errorDF_queryResults(print = TRUE)
#'
#' "P1761451"%>%query_UID_limslager()%>%get_errorDF_queryResults()
#' }
get_errorDF_queryResults <- function(query_result,
                                     error_string="error_",
                                     print = FALSE){

  error_df <- error <- NULL
  error <- query_result$error_message

  if(length(error)>0){
    isError <- detect_error_inCharacterVector(error)

    if(any(isError)){

      error_df <- split_errorText_toDF(error[isError])

      if(print){error_df%>%print_queryResult_error()}
    }
  }
  return(error_df)
}

#' returns position index of complete cases
#'
#' checks for every index if all list elements except the ID_column contain a error string.
#' Returns the index value of those rows where no error was found in all list variables.
#'
#' @param input list of equally sized elements potentially containing the string "error"
#' @param ID_column character of the ID list element to print but not to check
#' @param error_string character to find as error
#' @param print logical if a message should be printed
#'
#' @return integer vector of complete cases row-index
#' @export
#'
#' @keywords internal
#' @family develTools
#'
#' @examples
#'input=list(chip_ID=c("M583054","M1730416","rrr","tttt"),
#'           chip_path=c("error_no path to chip data found on the provided servers.",
#'           "rfrf","\\\\intern.chipcytometry.com\\imagedata\\volume20\\M1730416","lll"),
#'           test=c("TRUE","error","error_","jjii"))
#'ID_column="chip_ID"
#'error_string = "error"
#'get_nonError_index(input,ID_column)
#'
#'input=list(chip_ID=c("M583054","M1730416"),
#'           chip_path=c("error_no path to chip data found on the provided servers.",
#'           "\\\\intern.chipcytometry.com\\imagedata\\volume20\\M1730416"))
#'get_nonError_index(input,ID_column)
get_nonError_index<- function(input,ID_column,error_string="error",print=TRUE){

  all_vars <- pos_to_check <- check_vars <- Exist <- Error <- error_vars <- pos_with_error <- complete_cases <- IDs_found <- NULL


  all_vars <-names(input)

  pos_to_check <- which(all_vars != ID_column)

  check_vars <- all_vars[pos_to_check]

  Exist <- purrr::map(check_vars,
                      ~input[.x]%>%
                        purrr::map(
                          ~stringr::str_detect(.x,error_string,negate=TRUE)))
  Error <- purrr::map(check_vars,
                      ~input[.x]%>%
                        purrr::map(
                          ~stringr::str_detect(.x,error_string)))

  error_vars <- purrr::map_lgl(Error,
                               ~any(.x%>%unlist()))

  pos_with_error <- which(error_vars)


  if(length(pos_with_error)>0){
    if(print){
      purrr::walk(pos_with_error,
                  ~writeLines(c(paste0("- ! NO ",check_vars[.x]," found for ",ID_column,": "),
                                paste0("  + ",input[[ID_column]][which(Error[[.x]]%>%unlist())]))))
    }
  }

  complete_cases <- which(Exist%>%dplyr::bind_cols()%>%t()%>%colSums==length(check_vars))


  if(length(complete_cases)>0){
    IDs_found <- input[[ID_column]][complete_cases]
    if(print){
      writeLines(c(paste0("- complete data found for ",ID_column,": "),
                   paste0("  + ",IDs_found)))
    }
  }else{if(print){writeLines("!!! no data found")}}

  return(complete_cases)

}

#_______________________
#handle_trycache_error()
#_______________________

#' standardizes try-chache error function
#'
#' creates a message string consisting of the error message produced in the try chache and a task defined by the user.
#' Writes the message into the console and into a textfile named by "error_message.txt", located in the \code{output_dir}.
#' if \code{enable.quit} is set \code{TRUE} R execution will quit without saving
#'
#' @param err character specifying the error generated and assigned by the tryCatch function
#' @param task character specifying the task, processed within the tryCatch
#' @param enable.quit logical if quit without saving
#'
#' @return prints a message into console and appends the error.txt-file in the \code{output_dir}
#'
#' @keywords internal
#' @family develTools
#'
#' @examples
#'
#' output.dir <- getwd()
#' enable.quit<- FALSE
#' task<-"test"
#'
#'testthat::expect_warning(
#' tryCatch({
#' log(-1)
#'  }, error = function(err) {handle_trycache_error(err,task,enable.quit)})
#')
#'testthat::expect_warning(
#' tryCatch({
#' task<-"test_2"
#' log(-1)
#'  }, error = function(err) {handle_trycache_error(err,task,enable.quit)})
#')
#'testthat::expect_warning(
#'  tryCatch({
#' log(-1)
#'  }, error = function(err) {handle_trycache_error(err,task,enable.quit)})
#')
handle_trycache_error <- function(err,task,enable.quit=FALSE,output_dir = getwd()) {

  V <- 080322
  #- set enable.quit to FALSE (wg globalVariable)
  msg <- NULL

  msg <- c(paste0("- An error occurred while ",task),
           paste0("- ERROR TEXT: ", err))
  writeLines(msg)
  sink(paste0(output_dir, "/error_message.txt"))
  writeLines(msg)
  sink(NULL)

  if (enable.quit){quit(save = "no", status = 666)}else{stop()}
}

#__________________
#is_error_in_EDLs()
#__________________

#' returns index which EDLs contains error
#'
#' applies check_EDLs_strings() and returns index if check resulted an error
#'
#' @param EDLs character vector of EDLs
#'
#' @return logical if EDLs is not readlable
#' @export
#'
#' @keywords internal
#' @family errorHandling
#'
#' @examples
#' testEDL <- c(
#' "<foo><bar /></foo>",
#' "<ObjRef UID=\"D1891203\" Type=\"Gate\"/>
#' <ObjRef  UID=\"D1614318\" Type=\"Gate\"/>
#' <ObjRef UID=\"D973219\" Type=\"Gate\"/>",
#' "lalala",NA,NULL)
#' testEDL%>%is_error_in_EDLs()

is_error_in_EDLs <- function(EDLs){
  EDLerros <- isError <- NULL

  EDLerrors <- check_EDLs_strings(EDLs,print=FALSE)
  isError <- detect_error_inCharacterVector(EDLerrors)
  return(isError)

}

#___________________________
#is_error_in_mappingResult()
#___________________________

#' checks for error messages in result list
#'
#' @param resultList a result list containing error messages, potentially
#'
#' @return logical if list element is a error
#' @export
#'
#' @keywords internal
#' @family errorHandling
#'
#' @examples
#' resultList <- list(df1 = data.frame(col1=c(1,2,3),col2=c(1,2,3)),
#' df2 = data.frame(col1=c(1,2,3),col2=c(1,2,3)),
#' error="error_text")
#'
#' resultList%>%is_error_in_mappingResult()
is_error_in_mappingResult <- function(resultList){

  isError <- isCharacter <- NULL

  isCharacter <- purrr::map_lgl(resultList,
                         ~checkmate::test_character(.x,len=1))

  isError<-purrr::map_lgl(resultList[isCharacter],
                          ~detect_error_inCharacterVector(.x))


  resultList[!isCharacter] <- FALSE
  resultList[isCharacter][!isError] <- FALSE
  resultList[isCharacter][isError] <- TRUE

  return(resultList)
}

#______________________
#split_errorText_toDF()
#______________________

#' splits a error_message and returns a dataframe
#'
#' takes a character vector of error_messages and splits each character by "_".
#' extracts the second and third split product.
#' returns a dataframe containing error_ID and error_text
#'
#' @param error character vector of error messages
#'
#' @return dataframe containing error_ID and error_text
#' @export
#'
#' @keywords internal
#' @family errorHandling
#'
#' @examples
#'
#' error <- c("error_some text_M0815","Error_some further text_M0819")
#' split_errorText_toDF(error)
#'
split_errorText_toDF <- function(error){

  error_df <- NULL
  error_df <- tibble::tibble(error_ID = stringr::str_split(error,"_",simplify = TRUE)[,3],
                             error_text = stringr::str_split(error,"_",simplify = TRUE)[,2])

  return(error_df)
}

#_________________________
#print_queryResult_error()
#_________________________

#' prints an overview of entities where mongoDB query serves no return
#'
#' @param error_df data.frame or NULL, output generated by get_errorDF_queryResults()
#'
#' @return print to console
#' @export
#'
#' @keywords internal
#' @family errorHandling
#'
#' @examples
#' \donttest{
#' query_result <- c("P1761451","re","t")%>%query_UID_limslager()
#' query_result%>%get_errorDF_queryResults()%>%print_queryResult_error
#' "P1761451"%>%query_UID_limslager()%>%
#' get_errorDF_queryResults()%>%print_queryResult_error()
#' }
print_queryResult_error <- function(error_df){
  error_text <- NULL

  if(is.null(error_df)==FALSE){

    writeLines(c("!!! NOTE in query mongoDB:"))

    error_df <- error_df%>%
      dplyr::group_by(error_text)

    error_df%>%
      dplyr::group_walk(~
                          writeLines(
                            c(paste0("- ",.y%>%unique()," :"),
                              c(paste0("  + ",.x%>%unlist())))))
  }
}
