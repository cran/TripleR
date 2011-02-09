\name{getEffects}
\alias{getEffects}

\title{
Calculates round robin effects for multiple variables
}
\description{
This is a helper function which facilitates the calculation of round robin effects if many variables are assessed. Only univariate analyses are possible at the moment.
}
\usage{
getEffects(formule, data, varlist, by=NA, na.rm=TRUE, ...)
}

\arguments{
  \item{formule}{The right hand side of the formula, specifying the actor, partner and group variable.
}
  \item{data}{
The data frame.
}
  \item{varlist}{
A vector with the column names (the column numbers are not possible!) of the variables which should be inserted at the left hand side of the formula.
}
  \item{by}{
By which variables should the results be merged? If by is NA (the default), a intelligent default handling is performed. It is strongly recommended to keep the defaults.
}
  \item{na.rm}{
This argument is passed to function \code{RR}.
}
  \item{...}{
Additional parameters passed to RR (e.g., selfenhance)
}

}

\value{
A data frame with all effects is returned
}
\author{
Felix Schönbrodt
}

\seealso{
\code{\link{RR}}
}

\examples{

data(likingLong)
res <- getEffects(~actor.id*partner.id, data=likingLong, varlist=c("liking_a", "liking_b", "metaliking_a", "metaliking_b"))
str(res)

}
