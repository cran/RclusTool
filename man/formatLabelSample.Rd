% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleBuilding.R
\name{formatLabelSample}
\alias{formatLabelSample}
\title{Labels formatting}
\usage{
formatLabelSample(
  label,
  data.sample,
  new.labels = TRUE,
  use.sampling = FALSE,
  noise.cluster = "Noise"
)
}
\arguments{
\item{label}{vector of labels.}

\item{data.sample}{sample object.}

\item{new.labels}{boolean: if TRUE (default), new names are given for each cluster (beginning by 'Cluster').}

\item{use.sampling}{boolean: if TRUE (not default), data.sample$sampling is used to generalize label from sampling set to the whole set.}

\item{noise.cluster}{character name of the cluster "noise".}
}
\value{
new.labels formatted labels.
}
\description{
Format labels for unsupervised classification and add cleaned observations as 'Noise'.
}
\details{
formatLabelSample formats labels for unsupervised classification and adds cleaned observations as 'Noise'
}
\examples{
dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
tf <- tempfile()
write.table(dat, tf, sep=",", dec=".")

x <- importSample(file.features=tf)
res <- KmeansQuick(x$features$initial$x, K=3)

new.labels <- formatLabelSample(res$cluster, x)


}
