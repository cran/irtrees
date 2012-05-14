\name{VerbAgg3}
\docType{data}
\alias{VerbAgg3}
\title{Verbal Aggression Data, Three-Category Items}
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
str(VerbAgg3)
mapping <- cbind(c(0,1,1), c(NA,0,1))
str(VerbAgg3T <- dendrify(VerbAgg3[,-c(1,2)], mapping))
}
\keyword{datasets}
