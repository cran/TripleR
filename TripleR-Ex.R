pkgname <- "TripleR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('TripleR')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("RR")
### * RR

flush(stderr()); flush(stdout())

### Name: RR
### Title: Triple R: Round-Robin Analyses Using R
### Aliases: RR liking_a likingLong multiLikingLong liking_b metaliking_a
###   metaliking_b multiGroup multiNarc
### Keywords: univar htest

### ** Examples

# The example data are taken from the "Mainz Freshman Study" and consist 
# of ratings of liking as well as ratings of the metaperception of 
# liking at zero-acquaintance using a Round-Robin group of 54 participants 

#------------------------------------------------------------
# ----  Single group   --------------------------------------
#------------------------------------------------------------

# Load data frame in long format - it contains 4 variables:
#liking ratings indicator a (liking_a, "How likeable do you find this person?")  
#liking ratings indicator b (liking_b, "Would you like to get to know this person?")
#metaliking ratings indicator a (metaliking_a, "How likeable does this person find you?")
#metaliking ratings indicator b (metaliking_b, "Would this person like to get to know you?")

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
# appropriate for perception-perception data  
# Note: you can use abbreviations of the strings "behavior", "perception", and "metaperception"
print(RR4, measure1="p", measure2="p") 



#------------------------------------------------------------
# ----  Multiple groups --------------------------------------
#------------------------------------------------------------

# data("multiLikingLong") is a variant of the liking data set (see above) with multiple groups
data("multiLikingLong")

# set RR.style to "perception" (affects subsequent printing of objects)
RR.style("perception")

#manifest univariate SRM analysis
RR1m <- RR(liking_a ~ actor.id*partner.id|group.id, data=multiLikingLong)

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


#------------------------------------------------------------
# ----  Multiple groups with missing values --------------------------------------
#------------------------------------------------------------

# a multi group data set with two variables:
# ex = extraversion ratings, and ne = neurotizism ratings
data("multiGroup")

#manifest univariate SRM analysis, data set with missings
RR1miss <- RR(ex~actor.id*partner.id|group.id, data=multiGroup, na.rm=TRUE)

#manifest univariate SRM analysis, data set with missings, 
# minimum 10 data points are requested for each participant
RR1miss <- RR(ex~actor.id*partner.id|group.id, data=multiGroup, na.rm=TRUE, minData=10)





cleanEx()
nameEx("RR.style")
### * RR.style

flush(stderr()); flush(stdout())

### Name: RR.style
### Title: Set labeling styles for RR analyses
### Aliases: RR.style

### ** Examples

data("likingLong")

RR.style("behavior")
RR(liking_a ~ actor.id*partner.id, data=likingLong)

RR.style("p")	# a "p" is enough for "perception"
RR(liking_a ~ actor.id*partner.id, data=likingLong)




cleanEx()
nameEx("RR.summary")
### * RR.summary

flush(stderr()); flush(stdout())

### Name: RR.summary
### Title: Print group descriptives
### Aliases: RR.summary

### ** Examples


data("multiGroup")
RR.summary(ex~actor.id*partner.id|group.id, data=multiGroup) 



cleanEx()
nameEx("getEffects")
### * getEffects

flush(stderr()); flush(stdout())

### Name: getEffects
### Title: Calculates round robin effects for multiple variables
### Aliases: getEffects

### ** Examples


data(likingLong)
res <- getEffects(~actor.id*partner.id, data=likingLong, varlist=c("liking_a", "liking_b", "metaliking_a", "metaliking_b"))
str(res)




cleanEx()
nameEx("long2matrix")
### * long2matrix

flush(stderr()); flush(stdout())

### Name: long2matrix
### Title: Convert long format to a quadratic matrix
### Aliases: long2matrix

### ** Examples


#load a data set in long style
data("multiGroup")

str(multiGroup)
qm <- long2matrix(ex~actor.id*partner.id|group.id, multiGroup)
qm[[2]]

# we see some warnings that some persons are only actors or only partners. Let's check the data without removing them:
qm2 <- long2matrix(ex~actor.id*partner.id|group.id, multiGroup, reduce=FALSE)
qm2[[2]]
 



cleanEx()
nameEx("matrix2long")
### * matrix2long

flush(stderr()); flush(stdout())

### Name: matrix2long
### Title: Convert a quadratic matrix to long format
### Aliases: matrix2long

### ** Examples

#The example data are taken from the "Mainz Freshman Study" and consist 
# of ratings of liking as well as ratings of the metaperception of 
# liking at zero-acquaintance using a Round-Robin group of 54 participants 
# (Back, Schmukle, & Egloff, in pres)

# load a data set in matrix style
data("liking_a")

str(liking_a)
long <- matrix2long(liking_a)

str(long)
 



cleanEx()
nameEx("plot.RRuni")
### * plot.RRuni

flush(stderr()); flush(stdout())

### Name: plot.RRuni
### Title: Plot variance components from SRAs
### Aliases: plot.RRuni plot.RRbi plot.RRmulti

### ** Examples

	data(likingLong)
	RR1 <- RR(liking_a ~ actor.id*partner.id, data=likingLong)
	plot(RR1)
	plot(RR1, geom="pie")
	
	RR2 <- RR(liking_a + metaliking_a ~ actor.id*partner.id, data=likingLong)
	plot(RR2)
	
	
	data("multiLikingLong")
	RR1m <- RR(liking_a ~ actor.id*partner.id|group.id, data=multiLikingLong)
	plot(RR1m)
	plot(RR1m, measure="perception")
	plot(RR1m, measure="perception", geom="bar")
	plot(RR1m, measure="perception", connect=TRUE)

	RR2m <- RR(liking_a + metaliking_a ~ actor.id*partner.id|group.id, data=multiLikingLong)
	plot(RR2m)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
