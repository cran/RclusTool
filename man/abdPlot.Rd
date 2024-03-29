% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleView.R
\name{abdPlot}
\alias{abdPlot}
\title{Abundances barplot}
\usage{
abdPlot(
  label,
  title,
  charsize = 11,
  point.param = c("grey", "black", "red", "blue", "green", "cyan", "yellow", "orange",
    "rosybrown", "palevioletred", "darkblue", "deeppink", "blueviolet", "darkgoldenrod1",
    "chartreuse", "darkorchid1", "deeppink", "coral", "darkolivegreen1", "#66C2A5",
    "#9DAE8C", "#D49A73", "#F08F6D", "#C79693", "#9E9DBA", "#9F9BC9", "#C193C6",
    "#E28BC3", "#D2A29F", "#BABF77", "#AAD852", "#CBD844", "#ECD836", "#FAD53E",
    "#F1CD64", "#E7C689", "#D7BF9C", "#C5B9A7", "#B3B3B3", "#D53E4F", "#E04F4A",
    "#EB6046", "#F47346", "#F88B51", 
     "#FBA35C", "#FDB869", "#FDCA79", "#FDDD88",
    "#F6E68F", "#EDEE93", "#E2F398", "#CDEA9D", "#B7E2A1", "#A0D8A4", "#86CEA4",
    "#6DC4A4", "#58B2AB", "#459DB4", "#3288BD")
)
}
\arguments{
\item{label}{factor describing the clustering.}

\item{title}{naming the graph.}

\item{charsize}{character size}

\item{point.param}{specifying the colors and the symbols to use for clusters display.}
}
\value{
None
}
\description{
Display the abundances barplot of a clustering.
}
\details{
abdPlot displays the abundances barplot of a clustering.
}
\examples{
dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
             matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
colnames(dat) <- c("x","y")
tf <- tempfile()
write.table(dat, tf, sep=",", dec=".")

x <- importSample(file.features=tf)

x <- computeUnSupervised(x, K=3, method.name='K-means')

abdPlot(x[["clustering"]][["K-means_preprocessed"]][["label"]], 'K-means_preprocessed')


}
\keyword{internal}
