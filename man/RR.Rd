\name{RR}
\alias{RR}
\alias{liking_a}
\alias{likingLong}
\alias{liking_b}
\alias{metaliking_a}
\alias{metaliking_b}
\alias{multiGroup}


\title{Triple R: Round-Robin Analyses Using R}
\description{
The function RR performs Social Relation Model (SRM) analyses for single or multiple round-robin groups. 
}
\usage{
RR(formula, data, na.rm=FALSE, verbose=TRUE, suffixes=NULL, g.id=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{a formula (see details) consisting of a measure (e.g. a rating) obtained with a round-robin group} 
\item{data}{A data frame in long format}
\item{na.rm}{Experimental! See Details}
\item{verbose}{Defines if warnings and additional information should be printed on execution}
\item{suffixes}{Which suffixes should be append to the actor and partner effects? Default is \code{.a} and \code{.p}. If you set the suffixes in one call of RR, they are stored as a permanent option (i.e., subsequent calls of RR without explicitly setting the suffixes will use the last setting).} 
\item{g.id}{For internal use only; do not set this parameter}
}
\details{
The estimation of the parameters of the Social Relation Model (SRM) is based on formulae provided by Kenny (1994; p. 236-244). For tests of significance of a single group, Triple R computes standard errors by using formulae published by Bond and Lashley (1996) for the case of a univariate SRM analysis. The formulae for the standard errors of the bivariate SRM parameters were kindly provided to us by C.F. Bond in personal communication. If multiple groups are provided, by default a between-group t-test is employed to calculate the significance. If you have very few groups with a considerable size (n>15), even in the multi group scenario it might be preferable to use the within-group test of significance. You can inspect the within-group test of significance for each of the multiple groups in the return value (see \code{groups.univariate}).

The formula to specify the round robin design has following notation:
\code{DV ~ actor.id * partner.id | group.id} (group.id is only provided if multiple groups are present). If two variables are used to describe one latent construct, both are connected with a slash on the left hand side: \code{DV1a/DV1b ~ actor.id * partner.id}. If two variables are used to describe two manifest constructs, both are connected with a \code{+} on the left hand side: \code{DV1+DV2 ~ actor.id * partner.id}. A latent analysis of two constructs would be notated as following: \code{DV1a/DV1b + DV2a/DV2b ~ actor.id * partner.id}.
If a formula is used, the parameter \code{analysis} is ignored, as it is inferred from the formula.


Data sets from the Mainz Freshman Study (see Back, Schmukle, & Egloff, 2009) are included (liking_a, liking_b, metaliking_a, metaliking_b, likingLong), as well as an additional data set containing missing values (multiGroup)

Warning! Experimental! - 
The handling for missing data (\code{na.rm=TRUE}) is performed in two steps:
\itemize{
\item rows and columns which are completely missing (e.g. if one member of the group did not show up) are removed from the matrix (i.e. both the "missing" row or column and the corresponding column or row, even if they have data in them)
\item the calculation for the sum of squares is adjusted by a correction factor that takes into account the altered degrees of freedom in the presence of missings. See also the included vignette, which documents the approach in more detail.
}
}

\value{
Printed are both unstandardized and standardized SRM estimates with the corresponding standard errors and t-values for actor variance, partner variance, relationship variance, error variance, actor-partner covariance, and relationship covariance. In case of a bivariate analysis values are additionally provided for actor-actor covariance, partner-partner covariance, actor-partner covariance, partner-actor covariance, intrapersonal relationship covariance, and interpersonal relationship covariance. In case of a latent SRM analysis standard errors and t-values are only printed for actor variance, partner variance, and relationship variance. Standard errors and t-values for the other parameters are not printed, because to our knowledge the corresponding formulae have not been developed yet. Therefore, we suggest using the t-values of the SRM analysis based on the mean of the two indicators by using a manifest SRM analysis.

\item{univariate}{List of results of univariate of SRM analyses - specify variable in index: univariate[[1]] or univariate[[2]]}
\item{bivariate}{Results of bivariate SRM analyses}
\item{effects}{actor and partner effects for all participants}
\item{effects.gm}{actor and partner effects for all participants, group mean added}
\item{effectsRel}{relationship effects for all participants}
\item{varComp}{variance components}
\item{group.var}{group variance estimate}

In the multi group case, also following values are returned:

\item{univariate}{The weighted average of univariate SRM results}
\item{bivariate}{The weighted average of bivariate SRM results}
\item{groups.univariate}{SRM results for each group}
\item{effects}{actor and partner effects for all participants}
\item{effectsRel}{relationship effects for all participants}
\item{varComp.group}{a list of variance components in all single groups (provided in the multigroup case)}

}



\note{Appropriate output labels for univariate analysis of behavior and perception data and for bivariate analyses including two kinds of behavior and/or perception data can be requested by using the \code{print} function. In case that a behavior was measured, the variances of an SRM analysis are labeled as actor variance, partner variance and relationship variance (default output labels). In case that a perception was measured, perceiver is printed instead of actor and target is printed instead of partner:

\itemize{
	\item \code{print(RRobject, measure1="behavior")}: prints output for a univariate analysis of behavior data.
	
	\item \code{print(RRobject, measure1="perception")}: prints output for a univariate analysis of perception data.

	\item \code{print(RRobject, measure1="behavior", measure2="behavior")}: prints output for a bivariate analysis of behavior data.
	
	\item \code{print(RRobject, measure1="perception", measure2="perception")}: prints output for a bivariate analysis of perception data.
	
	\item \code{print(RRobject, measure1="behavior", measure2="perception")} or \cr
	\code{print(RRobject, measure1="perception", measure2="behavior")}: prints output for a bivariate analysis of behavior and perception data.
	
	\item \code{print(RRobject, measure1="perception", measure2="metaperception")}: is for the special case that a perception and a corresponding metaperception was measured. In this case, additionally the appropriate output labels for bivariate SRM analyses of other- and metaperceptions (reciprocities, assumed reciprocities, meta-accuracies; see Kenny, 1994) are presented.
}
}

\references{ 
Back, M. D., Schmukle, S. C., & Egloff, B. (2009). \emph{From first sight to friendship: A longitudinal social relations analysis of stability and change in interpersonal attraction}. Manuscript submitted for publication. 

Kenny, D. A. (1994). \emph{Interpersonal perception: A social relations analysis}. New York: Guilford Press.

Lashley, B. R., & Bond, C. F., Jr. (1997). Significance testing for round robin data. \emph{Psychological Methods}, \emph{2}, 278-291.}

\author{Stefan C. Schmukle, Felix D. Schönbrodt & Mitja D. Back}

\examples{
#The example data are taken from the "Mainz Freshman Study" and consist 
# of ratings of liking as well as ratings of the metaperception of 
# liking at zero-acquaintance using a Round-Robin group of 54 participants 
# (Back, Schmukle, & Egloff, 2009)


#------------------------------------------------------------
# ----  Single group   --------------------------------------
#------------------------------------------------------------



#Four Round-Robin matrices are read
#liking ratings indicator a ("How likeable do you find this person?")  
data("liking_a")
#liking ratings indicator b ("Would you like to get to know this person?")
data("liking_b")
#metaliking ratings indicator a ("How likeable does this person find you?")
data("metaliking_a")
#metaliking ratings indicator b ("Would this person like to get to know you?")
data("metaliking_b")  

#all four variable in one data frame in long format
data("likingLong")


#manifest univariate SRM analysis
RR1 <- RR(liking_a ~ actor.id*partner.id, data=likingLong)

#manifest bivariate SRM analysis
RR2 <- RR(liking_a + metaliking_a ~ actor.id*partner.id, data=likingLong)

#latent (construct-level) univariate SRM analysis
RR3 <- RR(liking_a / liking_b ~ actor.id*partner.id, data=likingLong)

#latent (construct-level) bivariate SRM analysis
RR4 <- RR(liking_a/liking_b + metaliking_a/metaliking_b ~ actor.id*partner.id, data=likingLong)


# prints output of the manifest univariate analysis
# in terms of actor and partner variance (default output labels)
print(RR1, measure1="behavior") 

# prints output of the manifest univariate analysis 
# in terms of perceiver and target variance (appropriate for perception data)
print(RR1, measure1="perception")

# prints output of the manifest bivariate SRM analysis appropriate 
# for perception-metaperception data  
print(RR2, measure1="perception", measure2="metaperception")

#prints output of the latent univariate SRM analysis
# appropriate for perception data  
print(RR3, measure1="perception") 

#prints output of the latent bivariate SRM analysis
# appropriate for perception-metaperception data  
print(RR4, measure1="perception", measure2="metaperception") 



#------------------------------------------------------------
# ----  Multi group --------------------------------------
#------------------------------------------------------------


data("multiGroup")
data("multiLikingLong")

#manifest univariate SRM analysis
RR1m <- RR(liking_a ~ actor.id*partner.id|group.id, data=multiLikingLong)

#manifest univariate SRM analysis, data set with missings
RR1m <- RR(ex~actor.id*partner.id|group.id, data=multiGroup, na.rm=TRUE)

#manifest bivariate SRM analysis
RR2m <- RR(liking_a + metaliking_a ~ actor.id*partner.id|group.id, data=multiLikingLong)

#latent (construct-level) univariate SRM analysis
RR3m <- RR(liking_a / liking_b ~ actor.id*partner.id|group.id, data=multiLikingLong)

#latent (construct-level) bivariate SRM analysis
RR4m <- RR(liking_a/liking_b + metaliking_a/metaliking_b ~ actor.id*partner.id|group.id, data=multiLikingLong)

# prints output of the manifest univariate analysis
# in terms of actor and partner variance (default output labels)
print(RR1m, measure1="behavior") 

# prints output of the manifest univariate analysis 
# in terms of perceiver and target variance (appropriate for perception data)
print(RR1m, measure1="perception")

}

\keyword{univar}
\keyword{htest}
