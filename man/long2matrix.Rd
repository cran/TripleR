\name{long2matrix}
\alias{long2matrix}

\title{Convert long format to a quadratic matrix}
\description{
This function converts data from the long format to a quadratic round robin matrix. It is ensured that the matrix in fact is quadratic (i.e. rows or columns are added if necessary).
}

\usage{
long2matrix(formule, data, verbose=TRUE, reduce=TRUE, skip3=FALSE, g.id=NULL)
}

\arguments{
	\item{data}{A data frame in long format)}
	\item{formule}{A formula specifying the variable names. Example usage: \code{DV ~ actor.id*partner.id | group.id} (group.id only necessary in multi group data sets)}
	\item{verbose}{Should additional information be printed?}
	\item{reduce}{Should persons that only are actors or only partners be removed? Actor/partner effects and variance components can only be calculated, if every person is both an actor and a partner. For displaying reasons (e.g. for data inspection), however, it can be necessary to check, which persons are missing.}
	\item{skip3}{Should groups with 3 or fewer participants be skipped? SRAs need groups with minimum4 participants}
	\item{g.id}{For internal use only}
}

\value{
A list of quadratic matrix. Each list entry is one group, as specified by \code{group.id}. If only one group is present, this is stored in the first list entry.
}

\examples{
#The example data are taken from the "Mainz Freshman Study" and consist 
# of ratings of liking as well as ratings of the metaperception of 
# liking at zero-acquaintance using a Round-Robin group of 54 participants 
# (Back, Schmukle, & Egloff, 2009)


#all four variable in one data frame in long format
data("multiGroup")

str(multiGroup)
qm <- long2matrix(ex~actor.id*partner.id|group.id, multiGroup)
qm[[2]]

# we see some warnings that some persons are only actors or only partners. Let's check the data without removing them:
qm2 <- long2matrix(ex~actor.id*partner.id|group.id, multiGroup, reduce=FALSE)
qm2[[2]]


 
}