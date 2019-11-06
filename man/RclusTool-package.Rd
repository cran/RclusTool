\name{RclusTool}
\alias{RclusTool}
\docType{package}
\title{
Graphical Toolbox for Clustering and Classification of Data Frames
}
\description{
Graphical toolbox for clustering and classification of data frames. It proposes a graphical interface to process clustering and classification methods on features data-frames, and to view initial data as well as resulted cluster or classes. According to the level of available labels, different approaches are proposed: unsupervised clustering, semi-supervised clustering and supervised classification. To assess the processed clusters or classes, the toolbox can import and show some supplementary data formats: either profile/time series, or images. These added information can help the expert to label clusters (clustering), or to constrain data frame rows (semi-supervised clustering), using Constrained spectral embedding algorithm by Wacquet et al. (2013) <doi:10.1016/j.patrec.2013.02.003> and the methodology provided by Wacquet et al. (2013) <doi:10.1007/978-3-642-35638-4_21>.

There is a main command: \strong{RclusToolGUI()} to launch the graphical user interface of RclusTool. 

Other functions can be used in R scripts and are detailed in the documentation.
}
\details{

\tabular{ll}{
Package: \tab RclusTool \cr
Type: \tab Package \cr
Version: \tab 0.91 \cr
Date: \tab 2019-10-07 \cr
License: \tab GPL (>=2) \cr
LazyLoad: \tab yes\cr
}
}
\author{
Guillaume Wacquet [aut], 
Pierre-Alexandre Hebert [aut, cre], 
Emilie Poisson [aut], 
Pierre Talon [aut]

\strong{Maintainer:} Pierre-Alexandre Hebert \email{hebert@univ-littoral.fr}
}
\source{

"Constrained Spectral Embedding for K-Way Data Clustering." Pattern Recognition Letters, Wacquet, G., Caillault, E., Hamad, D., Hebert, P.-A. (2013) <doi:10.1016/j.patrec.2013.02.003>.

"Semi-supervised K-Way Spectral Clustering with Determination of Number of Clusters." Computational Intelligence: Revised and Selected Papers of the International Joint Conference, IJCCI 2011, Paris, France, October 24-26, 2011, Wacquet G., Poisson-Caillault E., Hebert PA. (2013) <doi:10.1007/978-3-642-35638-4_21>.

}

\keyword{clustering}
\keyword{graphical user interface}
\keyword{classification}
\keyword{semi-supervised}
\keyword{internal}
