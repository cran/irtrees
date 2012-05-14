\name{VerbAgg2}
\docType{data}
\alias{VerbAgg2}
\title{Verbal Aggression Data, Dichotomized Items}
\description{
Item responses to a questionaire on verbal aggression. 
These data are used throughout De Boeck and Wilson, 
Explanatory Item Response Models (Springer, 2004) to 
illustrate various forms of item response models.
}
\format{A data matrix with 316 persons, 24 three-category items,
and two person covariates (trait anger and gender).}
\source{http://bear.soe.berkeley.edu/EIRM/}
\references{De Boeck and Wilson (2004), Explanatory 
Item Response Models, Springer.
}
\examples{
str(VerbAgg2)
mapping <- cbind(c(1,1,1), diag(3))
str(VerbAgg2T <- exogenize(VerbAgg2[,-c(1,2)], mapping,
	endnode=rep(1:3, 8)))
}
\keyword{datasets}
