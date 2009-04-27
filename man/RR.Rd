\name{RR}
\alias{RR}
\alias{liking_a}
\alias{liking_b}
\alias{metaliking_a}
\alias{metaliking_b}


\title{Triple R: Round-Robin Analyses Using R}
\description{
The function RR performs Social Relation Model (SRM) analyses based on a single round-robin group. 
}
\usage{
RR(RRMatrix1, RRMatrix2 = NULL, RRMatrix3 = NULL, RRMatrix4 = NULL, analysis = "manifest")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{RRMatrix1}{an n x n matrix or data frame consisting of a measure (e.g. a rating) obtained with a round-robin group of n participants. Each row is one actor and each column is one partner. The diagonal is missing.} 
\item{RRMatrix2}{an n x n matrix or data frame consisting of a second measure in case of a bivariate and/or latent SRM analysis.}   
\item{RRMatrix3}{an n x n matrix or data frame consisting of a third measure in case of a bivariate latent SRM analysis.}
\item{RRMatrix4}{an n x n matrix or data frame consisting of a fourth measure in case of a bivariate latent SRM analysis.}
\item{analysis}{requests either a manifest SRM analysis with one measure per construct (\code{analysis = 'manifest'}) or a latent SRM analysis with two measures per construct (\code{analysis = 'latent'}). In case of a bivariate latent SRM analysis the two first matrices indicate the first construct and the third and fourth matrix indicate the second construct.} 
}
\details{
The estimation of the parameters of the Social Relation Model (SRM) is based on formulae provided by Kenny (1994; p. 236-244). For tests of significance, Triple R computes standard errors by using formulae published by Bond and Lashley (1996) for the case of a univariate SRM analysis. The formulae for the standard errors of the bivariate SRM parameters were kindly provided to us by C.F. Bond in personal communication.

Data sets from the Mainz Freshman Study (see Back, Schmukle, & Egloff, 2009) are included.
}

\value{
Printed are both unstandardized and standardized SRM estimates with the corresponding standard errors and t-values for actor variance, partner variance, relationship variance, error variance, actor-partner covariance, and relationship covariance. In case of a bivariate analysis values are additionally provided for actor-actor covariance, partner-partner covariance, actor-partner covariance, partner-actor covariance, intrapersonal relationship covariance, and interpersonal relationship covariance. In case of a latent SRM analysis standard errors and t-values are only printed for actor variance, partner variance, and relationship variance. Standard errors and t-values for the other parameters are not printed, because to our knowledge the corresponding formulae have not been developed yet. Therefore, we suggest using the t-values of the SRM analysis based on the mean of the two indicators by using a manifest SRM analysis.
}

\note{
Appropriate output labels for univariate analysis of behavior and perception data and for bivariate analyses including two kinds of behavior and/or perception data can be requested by using the \code{print} function. In case that a behavior was measured, the variances of an SRM analysis are labeled as actor variance, partner variance and relationship variance (default ouput labels). In case that a perception was measured, perceiver is printed instead of actor and target is printed instead of partner:

\itemize{
	\item{\code{print(RRobject, measure1="behavior")}}{prints output for a univariate analysis of behavior data.
	
	}
	
	\item{\code{print(RRobject, measure1="perception")}}{prints output for a univariate analysis of perception data.
	
	}
	
	\item{\code{print(RRobject, measure1="behavior", measure2="behavior")}}{prints output for a bivariate analysis of behavior data.
	
	}
	
	\item{\code{print(RRobject, measure1="perception", measure2="perception")}}{prints output for a bivariate analysis of perception data.
	
	}
	
	\item{\code{print(RRobject, measure1="behavior", measure2="perception")} or \code{print(RRobject, measure1="perception", measure2="behavior")}}{prints output for a bivariate analysis of behavior and perception data.
	
	}
	
	\item{\code{print(RRobject, measure1="perception", measure2="metaperception")}}{is for the special case that a perception and a corresponding metaperception was measured. In this case, additionally the appropriate output labels for bivariate SRM analyses of other- and metaperceptions (reciprocities, assumed reciprocities, meta-accuracies; see Kenny, 1994) are presented.}
}
}

\references{ 
Back, M. D., Schmukle, S. C., & Egloff, B. (2009). \emph{From first sight to friendship: A longitudinal social relations analysis of stability and change in interpersonal attraction}. Manuscript submitted for publication. 

Kenny, D. A. (1994). \emph{Interpersonal perception: A social relations analysis}. New York: Guilford Press.

Lashley, B. R., & Bond, C. F., Jr. (1997). Significance testing for round robin data. \emph{Psychological Methods}, \emph{2}, 278-291.}

\author{Stefan C. Schmukle, Felix D. Schoenbrodt & Mitja D. Back}

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }

\examples{
#The example data are taken from the "Mainz Freshman Study" and consist of ratings of liking as well as ratings of the 
#metaperception of liking at zero-acquaintance using a Round-Robin group of 54 participants (Back, Schmukle, & Egloff, 2009)

#Four Round-Robin matrices are read
data("liking_a") #liking ratings indicator a ("How likeable do you find this person?")  
data("liking_b") #liking ratings indicator b ("Would you like to get to know this person?")
data("metaliking_a") #metaliking ratings indicator a ("How likeable does this person find you?")
data("metaliking_b") #metaliking ratings indicator b ("Would this person like to get to know you?") 

RR1 <- RR(liking_a) #manifest univariate SRM analysis
RR2 <- RR(liking_a, metaliking_a, analysis="manifest") #manifest bivariate SRM analysis
RR3 <- RR(liking_a, liking_b, analysis="latent") #latent (construct-level) univariate SRM analysis
RR4 <- RR(liking_a, liking_b, metaliking_a, metaliking_b, analysis="latent") #latent (construct-level) bivariate SRM analysis

print(RR1, measure1="behavior") #prints output of the manifest univariate analysis in terms of actor and partner variance (default output labels)
print(RR1, measure1="perception") #prints output of the manifest univariate analysis in terms of perceiver and target variance (appropriate for perception data)
print(RR2, measure1="perception", measure2="metaperception") #prints output of the manifest bivariate SRM analysis appropriate for perception-metaperception data  
print(RR3, measure1="perception") #prints output of the latent univariate SRM analysis appropriate for perception data  
print(RR4, measure1="perception", measure2="metaperception") #prints output of the latent bivariate SRM analysis appropriate for perception-metaperception data  
}

\keyword{univar}
\keyword{htest}