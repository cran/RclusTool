% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleClustering.R
\name{sigClassif}
\alias{sigClassif}
\title{Signals clustering}
\usage{
sigClassif(data.sample, method, user.name = "")
}
\arguments{
\item{data.sample}{list containing features, profiles and clustering results.}

\item{method}{character vector specifying the clustering method (already performed) to use.}

\item{user.name}{character vector specifying the user name.}
}
\value{
signals plots images in the different directories.
}
\description{
Sort signals (if available) in different directories according to a clustering result.
}
\details{
sigClassif sorts signals (if available) in different directories according to a clustering result
}
\examples{
dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
tf1 <- tempfile()
write.table(dat, tf1, sep=",", dec=".")

sig <- data.frame(ID=rep(1:150, each=30), SIGNAL=rep(dnorm(seq(-2,2,length=30)),150))
tf2 <- tempfile()
write.table(sig, tf2, sep=",", dec=".")

dir.results <- tempdir()
x <- importSample(file.features=tf1,file.profiles = tf2, dir.save=dir.results)
x <- computeUnSupervised(x, K=3, method.name="K-means")

sigClassif(x, method = "K-means_preprocessed")
 

}
\seealso{
\code{\link{imgClassif}}
}
