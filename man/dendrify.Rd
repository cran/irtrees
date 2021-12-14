\name{dendrify}
\alias{dendrify}
\title{Measurement Mapping for an IRTree Model}
\description{
Expands a wide-form matrix of item responses to a long-form data frame of sub-item responses
}
\usage{
dendrify(mat, cmx)
}
\arguments{
  \item{mat}{An integer matrix of IRT responses (columns represent
    items, rows represent respondents)}
  \item{cmx}{A mapping matrix with as many rows as there are response options for the items.}
}
\value{A data frame with one row per sub-item response}
\references{
  De Boeck, P. and Partchev, I. (2012).
  IRTrees: Tree-Based Item Response Models of the GLMM Family,
  \emph{Journal of Statistical Software -- Code Snippets}, \bold{48}(1), 1--28.
  \url{https://www.jstatsoft.org/v48/c01/}.
}
\author{Ivailo Partchev}
\seealso{\code{\link{exogenize}}}
\examples{
mapping <- cbind(c(0,1,1), c(NA,0,1))
str(linrespT <- dendrify(linresp, mapping))
}
\keyword{models}
