\name{nesresp}
\docType{data}
\alias{nesresp}
\title{Example Data: Nested Hierarchical Latent Variables}
\description{
Simulated example data set with nested hierarchical categories.
}
\usage{data(nesresp)}
\format{An IRT response matrix with 300 persons and 10 four-category items.}
\references{
  De Boeck, P. and Partchev, I. (2012).
  IRTrees: Tree-Based Item Response Models of the GLMM Family,
  \emph{Journal of Statistical Software -- Code Snippets}, \bold{48}(1), 1--28.
  \url{http://www.jstatsoft.org/v48/c01/}.
}
\examples{
str(nesresp)
head(nesresp)
str(nesrespT <-
    dendrify(nesresp, cbind(c(0,0,1,1), c(0,1,NA,NA), c(NA,NA,0,1))))
head(nesrespT, 20)
}
\keyword{datasets}
