% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessTab.R
\name{initPreprocessTab}
\alias{initPreprocessTab}
\title{build Preprocess tab}
\usage{
initPreprocessTab(RclusTool.env, reset = FALSE, readConfig = FALSE)
}
\arguments{
\item{RclusTool.env}{environment in which data and intermediate results are stored.}

\item{reset}{boolean to reset the whole interface}

\item{readConfig}{boolean to reset the whole interface according the config parameters of the data.sample}
}
\value{
None
}
\description{
Generate the data preprocessing tab of the \code{\link{RclusToolGUI}}, in which the user can select, transform, filter or create variables.
}
\details{
function to initialize/refresh the 'preprocessTab' for data preprocessing (variables selection, transformation, creation, ...)
}
\keyword{internal}
