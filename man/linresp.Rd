\name{linresp}
\docType{data}
\alias{linresp}
\title{Example Data: Linear Hierarchical Categories}
\description{
Simulated example data set with linear hierarchical categories.
}
\format{An IRT response matrix with 300 persons and 10 three-category items.}
\references{
  De Boeck, P. and Partchev, I. (2012).
  IRTrees: Tree-Based Item Response Models of the GLMM Family,
  \emph{Journal of Statistical Software -- Code Snippets}, \bold{48}(1), 1--28.
  \url{http://www.jstatsoft.org/v48/c01/}.
}
\examples{
str(linresp)
mapping <- cbind(c(0,1,1), c(NA,0,1))
str(linrespT <- dendrify(linresp, mapping))
}
\keyword{datasets}
