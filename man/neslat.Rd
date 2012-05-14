\name{neslat}
\docType{data}
\alias{neslat}
\title{Example Data: Nested Hierarchical Categories}
\description{
Simulated example data set for a model with nested hierarchical latent variables.
}
\format{An IRT response matrix with 300 persons and 30 binary items.}
\references{
  De Boeck, P. and Partchev, I. (2012).
  IRTrees: Tree-Based Item Response Models of the GLMM Family,
  \emph{Journal of Statistical Software -- Code Snippets}, \bold{48}(1), 1--28.
  \url{http://www.jstatsoft.org/v48/c01/}.
}
\examples{
str(neslat)
str(neslatT <-
    exogenize(neslat, cbind(c(1,1,1), c(1,0,0), c(0,1,0), c(0,0,1)), 
	endnode=rep(1:3, each = 10)))
}
\keyword{datasets}
