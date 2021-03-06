% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_DBrelated.R
\name{get_EDL_from_query_result}
\alias{get_EDL_from_query_result}
\title{extracts all EDL strings from a mongoDB query result}
\usage{
get_EDL_from_query_result(result)
}
\arguments{
\item{result}{list containing the mongoDB result}
}
\value{
a character vector containing EDL strings
}
\description{
extracts all EDL strings from a mongoDB query result
}
\examples{
query_result <- list(
result = list(structure(list(
EDL = "<?xml version=\"1.0\"?>\r\n<!-- ",
EDLType = "ChipGroup"),
query = "UID_P1761451_limslager")),
error_message = character(0))

EDL <- query_result\%>\%
get_EDL_from_query_result()

}
\seealso{
Other database related: 
\code{\link{check_if_chip_data_exist}()},
\code{\link{connect_mongoDB}()},
\code{\link{extract_chipIDs_from_groupEDL}()},
\code{\link{find_chip_path}()},
\code{\link{find_server_path}()},
\code{\link{find_valid_group_chip_IDs}()}
}
\concept{database related}
\keyword{internal}
