\name{linlat}
\docType{data}
\alias{linlat}
\title{Example Data: Sequential-Order Latent Variable Models}
\description{
Simulated example data set for a model with sequential-order latent variable models.
}
\format{An IRT response matrix with 300 persons and 30 binary items.}
\references{
  De Boeck, P. and Partchev, I. (2012).
  IRTrees: Tree-Based Item Response Models of the GLMM Family,
  \emph{Journal of Statistical Software -- Code Snippets}, \bold{48}(1), 1--28.
  \url{http://www.jstatsoft.org/v48/c01/}.
}
\examples{
str(linlat)
str(linlatT <- exogenize(linlat, 
	cbind(c(1,0,0), c(1,1,0), c(0,1,0), c(0,1,1), c(0,0,1)),
    endnode = rep(1:3, each=10), cross = rep(1:10, 3)))
}
\keyword{datasets}
