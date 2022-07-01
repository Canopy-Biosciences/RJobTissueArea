#' chip_IDs of a chipgroup
#'
#' A vector containing those chip_IDs for which data could be found.
#'
#' \describe{
#'   \item{chip_IDs}{IDs of chips}
#' }
#'
#' @docType data
#' @usage data(chip_IDs)
#' @format a character vector of length 20
#' @keywords data
#' @describeIn data
#'
#' @source returned by package function: find_valid_group_chip_IDs(group_ID = "P1761451")
#' @name "chip_IDs"

#' mongoDB query result
#'
#' A list containing result dataframe of a query in limslager for 20 chip_IDs
#'
#' \describe{
#'   \item{result}{lists a data.frame, containing the mongoDB return object}
#'     \item{EDL}{column containing the EDL strings}
#'   \item{error_message}{vector containing text for each failed query}
#' }
#'
#' @docType data
#' @usage data(query_chipIDs_limslager)
#' @format list of a dataframe with 20 row and vector containing 0 error text
#' @keywords data
#' @describeIn data
#'
#' @source returned by package function: query_mongoDB("limslager","UID",chip_IDs)
#' @name "query_chipIDs_limslager"

#' EDL objects
#'
#' A vector containing the strings of 20 EDL objects, returned for 20 chip_IDs, queried in limslager
#'
#' \describe{
#'   \item{EDL_chipID_limslager}{vector containing EDL strings}
#' }
#'
#' @docType data
#' @usage data(EDL_chipIDs_limslager)
#' @format vector with 20 strings
#' @keywords data
#' @describeIn data
#'
#' @source returned by package function: query_chipIDs_limslager%>%get_EDL_from_query_result()
#' @name "EDL_chipIDs_limslager"

#' available servers
#'
#' A dataframe of available servers
#'
#' \describe{
#'   \item{server_path}{dataframe}
#'      \item{FlagEmpty}{0,1 indicating if server is online}
#'      \item{EDLName}{name of the server}
#'      \item{server_path}{path to the server}
#' }
#'
#' @docType data
#' @usage data(server_path)
#' @format dataframe with 25 rows
#' @keywords data
#' @describeIn data
#' @source returned by package function: find_server_path()
#' @name "server_path"

