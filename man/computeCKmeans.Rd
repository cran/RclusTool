% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleCompute.R
\name{computeCKmeans}
\alias{computeCKmeans}
\title{Constrained K-means clustering}
\usage{
computeCKmeans(
  x,
  K = 0,
  K.max = 20,
  mustLink = NULL,
  cantLink = NULL,
  maxIter = 2,
  kmeans.variance.min = 0.95
)
}
\arguments{
\item{x}{matrix of raw data (point by line).}

\item{K}{number of clusters. If K=0 (default), this number is automatically computed thanks to the Elbow method.}

\item{K.max}{maximal number of clusters (K.Max=20 by default).}

\item{mustLink}{list of ML (must-link) constrained pairs.}

\item{cantLink}{list of CNL (cannot-link) constrained pairs.}

\item{maxIter}{number of iterations for mpckm algorithm.}

\item{kmeans.variance.min}{elbow method cumulative explained variance > criteria to stop K-search.}
}
\value{
res.ckmeans results obtained from mpckm algorithm.
}
\description{
Perform Constrained K-means clustering, dealing with the number of clusters K, automatically or not.
}
\details{
computeCKmeans performs Constrained K-means clustering, dealing with the number of clusters K, automatically or not
}
\examples{
dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
tf <- tempfile()
write.table(dat, tf, sep=",", dec=".")
x <- importSample(file.features=tf)

ML=list(c(sel="10",mem="20"))
CNL=list(c(sel="1",mem="140"))

res.ckmeans <- computeCKmeans(x$features$initial$x, K=0, mustLink=ML, cantLink=CNL)

plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
col = res.ckmeans$label, main = "Constrained K-means clustering")


}
\seealso{
\code{\link{computeSemiSupervised}}, \code{\link{KwaySSSC}}
}
\keyword{internal}
