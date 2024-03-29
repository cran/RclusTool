% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleBuilding.R
\name{purgeSample}
\alias{purgeSample}
\title{Sample purging}
\usage{
purgeSample(
  data.sample,
  purge.preprocessing = TRUE,
  purge.clustering = TRUE,
  user.expert = FALSE
)
}
\arguments{
\item{data.sample}{sample object}

\item{purge.preprocessing}{boolean: if TRUE (default), the configuration is reset.}

\item{purge.clustering}{boolean: if TRUE (default), the clusterings are reset.}

\item{user.expert}{boolean : if FALSE (default), initial classification feature space is PCA.}
}
\value{
data.sample purged data.sample.
}
\description{
Purge sample from its temporary computing results.
}
\details{
Function to purgeSample from its temporary computing results
}
\examples{
dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
tf <- tempfile()
write.table(dat, tf, sep=",", dec=".")

x <- importSample(file.features=tf)
x <- computeUnSupervised(x, K=3, method.name="K-means")
x <- purgeSample(x, purge.clustering=TRUE)


}
