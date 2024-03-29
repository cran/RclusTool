% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleBuilding.R
\name{buildNameOperation}
\alias{buildNameOperation}
\title{Build Name Operation}
\usage{
buildNameOperation(formatedRow, lowercase = FALSE)
}
\arguments{
\item{formatedRow}{vector describing preprocessing operations}

\item{lowercase}{boolean to build lowercase operation name.}
}
\value{
name of a single operation
}
\description{
buildNameOperation build name of a single operation from its parameters
}
\examples{

formatedRow<-c(arg1="log", arg2="x", arg3="", arg4="")
buildNameOperation(formatedRow)

}
\keyword{internal}
