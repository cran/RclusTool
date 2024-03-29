% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters.R
\name{initParameters}
\alias{initParameters}
\title{Parameters initialization}
\usage{
initParameters(RclusTool.env = new.env())
}
\arguments{
\item{RclusTool.env}{environment in which data and intermediate results are stored.}
}
\value{
RclusTool.env new environment in which all global parameters, raw data and results are stored.
}
\description{
Initialize the global parameters and variables and generate a new environment used in the \code{\link{RclusToolGUI}}.
}
\details{
function to initialize the global parameters and variables
}
\examples{
RclusTool.env <- initParameters()

}
\keyword{internal}
