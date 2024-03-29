% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleCompute.R
\name{itemsModel}
\alias{itemsModel}
\title{Predictive models computation for the number of cells in colonies}
\usage{
itemsModel(dat, countFile, method = "mda")
}
\arguments{
\item{dat}{matrix or data.frame of raw data (points by line).}

\item{countFile}{character vector specifying the path and the name of the file containing manual countings.}

\item{method}{character vector specifying the name of method tu use for the building of predictive models. Must be 'lm', 'lda' or 'mda' (default).}
}
\value{
RDS file containing the predictive model.
}
\description{
Compute and save specific predictive model from manual countings for the estimation of number of cells in colonies.
}
\details{
itemsModel computes and saves specific predictive model from manual countings for the estimation of number of cells in colonies
}
\examples{
\donttest{
dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
tf <- tempfile()
write.table(dat, tf, sep=",", dec=".")
x <- importSample(file.features=tf)

x <- computeUnSupervised(x, K=0, pca=TRUE, echo=TRUE)

countFile <- countItemsSampleGUI(x)
if (file.exists(countFile))
     itemsModel(x$features$initial$x, countFile)
}
}
\seealso{
\code{\link{computeItemsSample}}, \code{\link{countItems}}
}
\keyword{internal}
