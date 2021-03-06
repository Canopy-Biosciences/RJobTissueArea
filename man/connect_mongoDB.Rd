% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_DBrelated.R
\name{connect_mongoDB}
\alias{connect_mongoDB}
\title{connect_mongoDB()}
\usage{
connect_mongoDB(mongo_collection, mongo_user = "chipcytouser")
}
\arguments{
\item{mongo_collection}{a character of the collection name ("limslager" oder "limsproc")}

\item{mongo_user}{character of username (chipcytouser)}
}
\value{
mongo_connection: a client to mongo_collection, providing functions
}
\description{
This Function connects you to collection of the ZKW mongoDB and returns a mongoDB client.
}
\details{
It connects to the database collection and returns an interface with functions. With these functions the content of the database can be accessed and extracted or changed.
}
\section{Available functions are: aggregate, count, distinct, drop, export, find, import, index, info, insert, iterate,}{
mapreduce, remove, rename, update.

The ZKW mongoDB consists of two main collections  "limslager" or "limsproc". Limslager stores metadata for real-world-objects and limsproc contains methods which were applied.

The data storage is organized hierarchically and the relationship between the objects and methods is realized by ID assignment.
The mongoDB client is build using the package mongolite 1.0. In newer versions, changes have been made that equire a newer mongoDB version than the one installed in the ZKW infrastructure.

You can easily install the required mongolite versions using remotes::install_version("mongolite","1.0")
connect_mongoDB() serves as a helperfunction for function query_mongoDB().
}

\examples{
\donttest{
connect_mongoDB("limslager")
}
}
\seealso{
Other database related: 
\code{\link{check_if_chip_data_exist}()},
\code{\link{extract_chipIDs_from_groupEDL}()},
\code{\link{find_chip_path}()},
\code{\link{find_server_path}()},
\code{\link{find_valid_group_chip_IDs}()},
\code{\link{get_EDL_from_query_result}()}
}
\concept{database related}
\keyword{internal}
