% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleBuilding.R
\name{makeFeatureSpaceOperations}
\alias{makeFeatureSpaceOperations}
\title{Make operation config object to build feature spaces}
\usage{
makeFeatureSpaceOperations(
  pca = FALSE,
  pca.nb.dims = 0,
  spectral = FALSE,
  sampling = FALSE,
  sampling.size.max = 0,
  scaling = FALSE
)
}
\arguments{
\item{pca}{boolean: if TRUE, Principal Components Analysis is applied to reduce the data space.}

\item{pca.nb.dims}{number of principal components kept. If pca.nb.dims=0, this number is computed automatically.}

\item{spectral}{boolean: if TRUE, spectral embedding is applied to reduce the data space.}

\item{sampling}{boolean: if TRUE, data sampling is used.}

\item{sampling.size.max}{: maximum size of the sampled dataframe.}

\item{scaling}{boolean: if TRUE, scaling is applied.}
}
\value{
configuration object created from parameters.
}
\description{
makeFeatureSpaceOperations create config object for datasample
}
\examples{

operations <- makeFeatureSpaceOperations(pca=TRUE)

}
\keyword{internal}
