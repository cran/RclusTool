% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codeSpectral.R
\name{bipartitionShi}
\alias{bipartitionShi}
\title{Spectral clustering}
\usage{
bipartitionShi(sim)
}
\arguments{
\item{sim}{similarity matrix.}
}
\value{
The function returns a list containing:
\item{label}{vector of labels.}
\item{eigenvector}{matrix containing, in columns, the eigenvectors of the similarity matrix.}
\item{eigenvalue}{vector containing the eigenvalues of the similarity matrix.}
}
\description{
Perform spectral clustering thanks to a similarity matrix (according to Shi and Malik, 2000).
}
\details{
bipartitionShi returns a partition obtained by spectral clustering (according to Shi and Malik, 2000)
}
\examples{
dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
           matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2))

sim <- computeGaussianSimilarity(dat, 1)
res <- bipartitionShi(sim)

plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
col = res$label, main = "Initial features space")
plot(res$eigenvector, type = "p", xlab = "Indices", ylab = "1st eigenvector", 
col = res$label, main = "Spectral embedding")
}
\references{
J. Shi, J. Malik, Normalized cuts and image segmentation, IEEE Transactions on Pattern Analysis and Machine Intelligence, 2000, 22(8), 888-905.
}
\keyword{internal}
