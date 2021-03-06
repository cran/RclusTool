% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleClustering.R
\name{readTrainSet}
\alias{readTrainSet}
\title{Training set reading}
\usage{
readTrainSet(
  traindir,
  keep_ = FALSE,
  operations = NULL,
  RclusTool.env = initParameters()
)
}
\arguments{
\item{traindir}{character vector specifying the path of the training set.}

\item{keep_}{boolean: if FALSE (default), the '_' directory is not considered in the training set.}

\item{operations}{list of data.frames describing all preprocessing operations.}

\item{RclusTool.env}{environment in which all global parameters, raw data and results are stored.}
}
\value{
prototypes data.frame containing the features of each prototype associated to a class.
}
\description{
Read a training set built from prototypes, to train a classifier for supervised classification.
}
\details{
readTrainSet reads a training set built from prototypes, to train a classifier for supervised classification
}
\examples{

rep <- system.file("extdata", package="RclusTool")
traindir <- file.path(rep, "train_example")
train <- readTrainSet(traindir)

}
\seealso{
\code{\link{dropTrainSetVars}}
}
