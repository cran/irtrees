\name{exogenize}
\alias{exogenize}
\title{Structural Mapping for an IRTree Model}
\description{
Expands a wide-form file of item responses to a long-form file supporting structural mappings among latent variables
}
\usage{
exogenize(mat, cmx, items=seq_len(ncol(mat)), endnode, crossitem=NULL)
}
\arguments{
  \item{mat}{An integer matrix of IRT responses (columns represent
    items, rows represent respondents)}
  \item{cmx}{The mapping matrix.}
  \item{items}{A numeric vector with the column positions of all items
    (measurement variables) in \code{mat}.  Defaults to all columns of \code{mat}.}
  \item{endnode}{A factor with the same length as \code{items}
    indicating the latent variable to which the item is attached, and
    compatible with the mapping matrix.}
  \item{crossitem}{A factor with the same length as \code{items}
    indicating the original items that have been crossed with
    \code{endnode} (e.g., given at various occasions over time), thus
    producing the actual items. This is not meaningful when the design
    is nested, and should better be omitted in such cases.}
}
\value{A data frame with columns:
  \item{person}{a factor identifying the respondent;} 
  \item{item}{a factor identifying the items;}
%  \item{exofactor}{a factor identifying the internal nodes from $s=1$ to $s=S$
%    and thus the exogeneous latent variables, with contrasts defined by \code{cmx};}
  \item{exo1 to exoS}{dummy variables identifying the internal nodes: these play the same role as \code{exofactor} but are necessary when the model must include independent random effects;} 
  \item{endnode}{a factor identifying the end nodes and thus the endogeneous latent variables;}
  \item{crossitem}{a factor produced only for crossed designs, identifying the items that are crossed 
    with \code{endnode}, for example, 
    in the case when the same items are repeated over time in a longitudinal design;} 
  \item{value}{the binary item responses.}
}
\references{
  De Boeck, P. and Partchev, I. (2012).
  IRTrees: Tree-Based Item Response Models of the GLMM Family,
  \emph{Journal of Statistical Software -- Code Snippets}, \bold{48}(1), 1--28.
  \url{http://www.jstatsoft.org/v48/c01/}.
}
\author{Ivailo Partchev}
\seealso{\code{\link{dendrify}}}
\examples{
str(linlatT <- exogenize(linlat, 
   cbind(c(1,0,0), c(1,1,0), c(0,1,0), c(0,1,1), c(0,0,1)),
   endnode = rep(1:3, each=10), cross = rep(1:10, 3)))
}
\keyword{models}
