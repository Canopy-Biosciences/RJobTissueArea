% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_errorHandling.R
\name{get_nonError_index}
\alias{get_nonError_index}
\title{returns position index of complete cases}
\usage{
get_nonError_index(input, ID_column, error_string = "error", print = TRUE)
}
\arguments{
\item{input}{list of equally sized elements potentially containing the string "error"}

\item{ID_column}{character of the ID list element to print but not to check}

\item{error_string}{character to find as error}

\item{print}{logical if a message should be printed}
}
\value{
integer vector of complete cases row-index
}
\description{
checks for every index if all list elements except the ID_column contain a error string.
Returns the index value of those rows where no error was found in all list variables.
}
\examples{
input=list(chip_ID=c("M583054","M1730416","rrr","tttt"),
          chip_path=c("error_no path to chip data found on the provided servers.",
          "rfrf","\\\\\\\\intern.chipcytometry.com\\\\imagedata\\\\volume20\\\\M1730416","lll"),
          test=c("TRUE","error","error_","jjii"))
ID_column="chip_ID"
error_string = "error"
get_nonError_index(input,ID_column)

input=list(chip_ID=c("M583054","M1730416"),
          chip_path=c("error_no path to chip data found on the provided servers.",
          "\\\\\\\\intern.chipcytometry.com\\\\imagedata\\\\volume20\\\\M1730416"))
get_nonError_index(input,ID_column)
}
\seealso{
Other develTools: 
\code{\link{handle_trycache_error}()}
}
\concept{develTools}
\keyword{internal}
