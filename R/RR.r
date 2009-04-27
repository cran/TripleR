#------------------------------------
#-- Copyright Information 
#
#
#
#
#
#
#------------------------------------


#------------------------------------
#-- Skinning variables
#------------------------------------

# Labels actor-partner
unilabels_b <- c("actor variance", "partner variance", "relationship variance", "error variance", "actor-partner covariance", "relationship covariance")
# Labels target-perceiver
unilabels_p <- c("perceiver variance", "target variance", "relationship variance", "error variance", "perceiver-target covariance", "relationship covariance")

unilabels2 <- c("estimate", "standardized", "se", "t.value")

# labels for metaperception
unilabels_b_meta1 <- c("perceiver variance otherperception", "target variance otherperception",  "relationship variance otherperception", "error variance otherperception", "generalized reciprocity otherperception", "dyadic reciprocity otherperception")
unilabels_b_meta2 <- c("perceiver variance metaperception", "target variance metaperception", "relationship variance metaperception", "error variance metaperception", "generalized reciprocity metaperception", "dyadic reciprocity metaperception")

# Labels for bivariate analyses
bilabels_bb <- c("actor-actor covariance","partner-partner covariance",
"actor-partner covariance","partner-actor covariance","intrapersonal relationship covariance", "interpersonal relationship covariance")
bilabels_pp <- c("perceiver-perceiver covariance","target-target covariance",
"perceiver-target covariance","target-perceiver covariance","intrapersonal relationship covariance", "interpersonal relationship covariance")
bilabels_bp <- c("actor-perceiver covariance","partner-target covariance",
"actor-target covariance","partner-perceiver covariance","intrapersonal relationship covariance", "interpersonal relationship covariance")
bilabels_pb <- c("perceiver-actor covariance","target-partner covariance",
"perceiver-partner covariance","target-actor covariance","intrapersonal relationship covariance", "interpersonal relationship covariance")


bilabels_meta <- c("Perceiver assumed reciprocity","Generalized assumed reciprocity",
"Perceiver meta-accuracy", "Generalized meta-accuracy", "Dyadic assumed reciprocity", "Dyadic meta-accuracy")






# calculates Actor-, Partner- and Relationship-Effects from a single RR-Matrix
RR.effects <- function(RRMatrix) {
	#Variance estimation

	 mip <- apply(RRMatrix, 1, mean, na.rm=TRUE)
	 mpj <- apply(RRMatrix, 2, mean, na.rm=TRUE)
	 mpp <- mean(as.matrix(RRMatrix), na.rm=TRUE)

	 n <- length (mip)
	 a <- ((n-1)^2)/(n*(n-2))*mip + (n-1)/(n*(n-2))*mpj - (n-1)/(n-2)*mpp #actor effects
	 b <- ((n-1)^2)/(n*(n-2))*mpj + (n-1)/(n*(n-2))*mip - (n-1)/(n-2)*mpp #partner effects

	 am <- matrix(a, nrow(RRMatrix), ncol(RRMatrix))
	 bm <- t(matrix(b, nrow(RRMatrix), ncol(RRMatrix)))
	 c <- as.matrix(RRMatrix - am - bm - mpp) # relationship effect
	
	return(list(actor = a, partner = b, relationship = c))
}



# calculates variance components from a single RR-Matrix
# na.rm = TRUE / FALSE / "impute" (<- not implemented yet)
RR.univariate <- function(RRMatrix, na.rm=FALSE) {
	
	# emit some warnings about missings if there are NAs outside the diagonale
	if ((sum(is.na(RRMatrix)) > nrow(RRMatrix)) & na.rm==FALSE)
		stop("There are NAs outside the diagonale. Calculations are aborted.")
		
	if ((sum(is.na(RRMatrix)) > nrow(RRMatrix)) & na.rm==TRUE) {
		#print("Note: There are NAs outside the diagonale. Degrees of freedom in Kenny's (1994) formula are adjusted for missings. Maybe you should think about imputation procedures to obtain a complete matrix.")
		stop("There are NAs outside the diagonale. Calculations are aborted.")
	}
	
	eff <- RR.effects(RRMatrix)
	n <- nrow(RRMatrix)
	
	A <- sum(eff$actor^2)/(n-1)
	B <- sum(eff$partner^2)/(n-1)
	C <- sum(eff$actor*eff$partner)/(n-1)
	e <- 0.5*(eff$relationship + t(eff$relationship))
	d <- eff$relationship - t(eff$relationship)
	
	D <- sum(e^2, na.rm=TRUE)/(((n-1)*(n-2)/2)-1)
	E <- (sum(d^2,na.rm=TRUE)/2)/((n-1)*(n-2))
	
	
	scc <- (D+E)/2 #relationship variance
	sccs <- (D-E)/2 #relationship covariance
	sab <- C - (sccs*(n-1))/(n*(n-2)) - scc/(n*(n-2)) #actor-partner covariance
	saa <- A - (scc*(n-1))/(n*(n-2)) - sccs/(n*(n-2)) #actor variance
	sbb <- B - (scc*(n-1))/(n*(n-2)) - sccs/(n*(n-2)) #partner variance
	raa <- saa/(saa + sbb + scc) #standardized actor variance
	rbb <- sbb/(saa + sbb + scc) #standardized partner variance
	rcc <- scc/(saa + sbb + scc) #standardized relationship variance
	rab <- sab/sqrt(saa*sbb) #actor-partner correlation
	rccs <- sccs/scc #relationship correlation
	
	
	w = (n^2 - 3*n + 6) * (n^2 - 3*n + 4)

	sesaa <- sqrt((2*saa^2) / (n+1) + (2*(n^6 - 7*n^5 + 28*n^4 - 66*n^3 + 102*n^2 - 84*n + 32)* scc^2)/ (w*(n+1)*n^2*(n-2)^2)
	        + (2*(n^3-n^2-2*n+16)*(n^2-2*n+2)*sccs^2) / (w*(n+1)*n^2*(n-2)^2)
	        + (4*saa*((n-1) * scc + sccs)) / ((n+1)*n*(n-2))
	        + (4*(n^5-5*n^4+20*n^3-42*n^2+60*n-32)*scc*sccs) / (w*(n+1)*n^2*(n-2)^2))

	sesbb <- sqrt((2*sbb^2) / (n+1) + (2*(n^6 - 7*n^5 + 28*n^4 - 66*n^3 + 102*n^2 - 84*n + 32)* scc^2)/ (w*(n+1)*n^2*(n-2)^2)
	        + (2*(n^3-n^2-2*n+16)*(n^2-2*n+2)*sccs^2) / (w*(n+1)*n^2*(n-2)^2)
	        + (4*sbb*((n-1) * scc + sccs)) / ((n+1)*n*(n-2))
	        + (4*(n^5-5*n^4+20*n^3-42*n^2+60*n-32)*scc*sccs) / (w*(n+1)*n^2*(n-2)^2))

	sescc <- sqrt((2*(n^2-3*n+5)* ((scc^2 + sccs^2))) / w + (4*scc*sccs)/ w)

	sesab <- sqrt(((n-3)*sab^2) / ((n+1)*(n-2)) + ((n^6 - 5*n^5 + 19*n^4 - 45*n^3 + 90*n^2 - 96*n + 64)* scc^2)/ (w*(n+1)*n^2*(n-2)^2)
	        + ((n^6 - 7*n^5 + 31*n^4 - 83*n^3 + 150*n^2 - 144*n + 64)* sccs^2)/ (w*(n+1)*n^2*(n-2)^2)
	        + ((n-1)*saa*sbb)/((n+1)*(n-2))
	        + ((n-1)*(saa+sbb)*((n-1)*scc+sccs))/((n+1)*n*(n-2)^2)
	        + (2*(n-3)*sab*(scc+(n-1)*sccs))/((n+1)*n*(n-2)^2)
	        + (4*(n^5-5*n^4+20*n^3-42*n^2+60*n-32)*scc*sccs) / (w*(n+1)*n^2*(n-2)^2))

	sesccs <- sqrt((2*(n^2-3*n+5)*((scc^2+sccs^2)))/w + (4*scc*sccs)/w)

	taa <- saa/sesaa
	tbb <- sbb/sesbb
	tcc <- scc/sescc
	tab <- sab/sesab
	tccs <- sccs/sesccs

	# error variance is NA if only one group is present
	estimate <- c(saa,sbb,scc,NA,sab,sccs)
	standardized <- c(raa,rbb,rcc,NA,rab,rccs)
	se <- c(sesaa,sesbb,sescc,NA,sesab,sesccs)
	t.value <- c(taa,tbb,tcc,NA,tab,tccs)

	univariate <- data.frame(estimate, standardized, se, t.value)
	rownames(univariate) <- unilabels_b

	res <- list(effect = eff, univariate = univariate, relMat.av=e, relMat.diff=d)
	class(res) <- "RR"

	return(res)
	
}



# combines two RR-matrices, depending on parameter 'latent'
# latent = TRUE: both matrices are treated as two measures for one underlying construct
# latent = FALSE: both matrices are treated as independent variables
RR.bivariate <- function(RRMatrix1, RRMatrix2, meta=TRUE, analysis="manifest", na.rm=FALSE) {
	
	if (!(analysis %in% c("latent", "manifest"))) stop("Parameter 'analysis' must either be 'latent' or 'manifest'. Calculations aborted.")
	
	RR.1 <- RR.univariate(RRMatrix1, na.rm)
	RR.2 <- RR.univariate(RRMatrix2, na.rm)	
	varComp.1 <- RR.1$univariate$estimate
	varComp.2 <- RR.2$univariate$estimate
	n <- nrow(RRMatrix1)

	#Bivariate Relationships

	A <- sum(RR.1$effect$actor*RR.2$effect$actor)/(n-1)
	B <- sum(RR.1$effect$actor*RR.2$effect$partner)/(n-1)
	C <- sum(RR.1$effect$partner*RR.2$effect$actor)/(n-1)
	D <- sum(RR.1$effect$partner*RR.2$effect$partner)/(n-1)
	E <- sum(RR.1$relMat.av*RR.2$relMat.av,na.rm=TRUE)/(((n-1)*(n-2)/2)-1)
	F <- (sum(RR.1$relMat.diff*RR.2$relMat.diff,na.rm=TRUE)/2)/((n-1)*(n-2))
	sch <- (E+F)/2 #intrapersonal relationship covariance
	schs <- (E-F)/2 #interpersonal relationship covariance
	saf <- A - (sch*(n-1))/(n*(n-2)) - schs/(n*(n-2)) #actor-actor covariance
	sag <- B - (schs*(n-1))/(n*(n-2)) - sch/(n*(n-2)) #actor-partner covariance
	sbf <- C - (schs*(n-1))/(n*(n-2)) - sch/(n*(n-2)) #partner-actor covariance
	sbg <- D - (sch*(n-1))/(n*(n-2)) - schs/(n*(n-2)) #partner-partner covariance
	raf <- saf/sqrt(varComp.1[1]*varComp.2[1]) #correlations
	rag <- sag/sqrt(varComp.1[1]*varComp.2[2])
	rbf <- sbf/sqrt(varComp.1[2]*varComp.2[1])
	rbg <- sbg/sqrt(varComp.1[2]*varComp.2[2])
	rch <- sch/sqrt(varComp.1[3]*varComp.2[3])
	rchs <- schs/sqrt(varComp.1[3]*varComp.2[3])
	
	
	if (analysis=="latent") {
		stabpervar1 <- saf
		stabtarvar1 <- sbg
		stabrelvar1 <- sch
		stabapcov1 <- (sag + sbf)/2
		stabdycov1 <- schs
		unstabper1 <- (varComp.1[1] + varComp.2[1])/2 - saf
		unstabtar1 <- (varComp.1[2] + varComp.2[2])/2 - sbg
		unstabrel1 <- (varComp.1[3] + varComp.2[3]) / 2 - sch
		stable1 <- saf + sbg + sch
		unstable1 <- unstabper1 + unstabtar1 + unstabrel1
		stabler1 <- stable1 / (stable1 + unstable1)
		unstabler1 <- unstable1 / (stable1 + unstable1)
		stabperr1 <- stabpervar1/(stable1 + unstable1)
		stabtarr1 <- stabtarvar1/(stable1+unstable1)
		stabrelr1 <- stabrelvar1/(stable1+unstable1)
		stabdycor1 <- stabdycov1/sch
		stabapcor1 <- stabapcov1 / sqrt(saf*sbg)
	}
	

	# Standard errors (se) und t-values (t) of bivariate srm-parameters

	coef1 <- n^11-14*n^10+89*n^9-342*n^8+872*n^7-1505*n^6+1698*n^5-1063*n^4+116*n^3+292*n^2-224*n+64
	coef2 <- n^10-12*n^9+66*n^8-227*n^7+534*n^6-857*n^5+883*n^4-416*n^3-148*n^2+224*n-64
	coef3 <- n^10-11*n^9+48*n^8-93*n^7-2*n^6+388*n^5-763*n^4+572*n^3+4*n^2-224*n+64
	coef4 <- n^11-16*n^10+117*n^9-520*n^8+1540*n^7-3083*n^6+3970*n^5-2689*n^4-4*n^3+1212*n^2-544*n-64
	coef5 <- n^10-11*n^9+42*n^8-33*n^7-258*n^6+976*n^5-1453*n^4+788*n^3+348*n^2-544*n-64
	coef6 <- 2*(n^10-14*n^9+92*n^8-383*n^7+1074*n^6-1963*n^5+2101*n^4-752*n^3-780*n^2+544*n+64)
	coef7 <- (n-3)*n*(n^6-9*n^5+35*n^4-75*n^3+76*n^2-12*n-48)

	sesaf <- sqrt(((n-1)*varComp.1[1]*varComp.2[1]+(n-3)*saf^2)/((n-2)*(n+1))
	         + ((n-1)^2*(varComp.1[1]*varComp.2[3]+varComp.1[3]*varComp.2[1])+(n-1)*(varComp.1[1]*varComp.2[5]+varComp.1[5]*varComp.2[1])+2*(n-3)*saf*((n-1)*sch+schs))/((n-2)^2*n*(n+1))
	         + (coef1*varComp.1[3]*varComp.2[3]+coef2*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+coef3*varComp.1[5]*varComp.2[5]+coef4*sch^2+coef5*schs^2+coef6*sch*schs)/((n-2)^3*n^2*(n+1)*coef7))

	sesbg <- sqrt(((n-1)*varComp.1[2]*varComp.2[2]+(n-3)*sbg^2)/((n-2)*(n+1))
	         + ((n-1)^2*(varComp.1[2]*varComp.2[3]+varComp.1[3]*varComp.2[2])+(n-1)*(varComp.1[2]*varComp.2[5]+varComp.1[5]*varComp.2[2])+2*(n-3)*sbg*((n-1)*sch+schs))/((n-2)^2*n*(n+1))
	         + (coef1*varComp.1[3]*varComp.2[3]+coef2*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+coef3*varComp.1[5]*varComp.2[5]+coef4*sch^2+coef5*schs^2+coef6*sch*schs)/((n-2)^3*n^2*(n+1)*coef7))

	sesch <- sqrt(((n^6-9*n^5+32*n^4-57*n^3+43*n^2+6*n-8)*(varComp.1[3]*varComp.2[3]+varComp.1[5]*varComp.2[5])+2*(n^4-6*n^3+3*n^2+18*n-8)*sch*schs)/coef7
	         + ((n^4-6*n^3+11*n^2-6*n+8)*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+(n^6-9*n^5+28*n^4-33*n^3-9*n^2+54*n+8)*(sch^2 + schs^2))/coef7)

	sesag <- sqrt(((n-1)*varComp.1[1]*varComp.2[2]+(n-3)*sag^2)/((n-2)*(n+1))
	         + ((n-1)^2*(varComp.1[1]*varComp.2[3]+varComp.1[3]*varComp.2[2])+(n-1)*(varComp.1[1]*varComp.2[5]+varComp.1[5]*varComp.2[2])+2*(n-3)*sag*((n-1)*schs+sch))/((n-2)^2*n*(n+1))
	         + (coef1*varComp.1[3]*varComp.2[3]+coef2*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+coef3*varComp.1[5]*varComp.2[5]+coef4*schs^2+coef5*sch^2+coef6*sch*schs)/((n-2)^3*n^2*(n+1)*coef7))

	sesbf <- sqrt(((n-1)*varComp.1[2]*varComp.2[1]+(n-3)*sbf^2)/((n-2)*(n+1))
	         + ((n-1)^2*(varComp.1[2]*varComp.2[3]+varComp.1[3]*varComp.2[1])+(n-1)*(varComp.1[2]*varComp.2[5]+varComp.1[5]*varComp.2[1])+2*(n-3)*sbf*((n-1)*schs+sch))/((n-2)^2*n*(n+1))
	         + (coef1*varComp.1[3]*varComp.2[3]+coef2*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+coef3*varComp.1[5]*varComp.2[5]+coef4*schs^2+coef5*sch^2+coef6*sch*schs)/((n-2)^3*n^2*(n+1)*coef7))

	seschs <- sqrt(((n^6-9*n^5+32*n^4-57*n^3+43*n^2+6*n-8)*(varComp.1[3]*varComp.2[3]+varComp.1[5]*varComp.2[5])+2*(n^4-6*n^3+3*n^2+18*n-8)*sch*schs)/coef7
	         + ((n^4-6*n^3+11*n^2-6*n+8)*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+(n^6-9*n^5+28*n^4-33*n^3-9*n^2+54*n+8)*(sch^2 + schs^2))/coef7)


	taf <- saf/sesaf
	tbg <- sbg/sesbg
	tch <- sch/sesch
	tag <- sag/sesag
	tbf <- sbf/sesbf
	tchs <- schs/seschs
	
	if (analysis=="latent") {
		sestabpervar1 <- sesaf
		sestabtarvar1 <- sesbg
		sestabrelvar1 <- sesch

		tstabpervar1 <- saf/sesaf
		tstabtarvar1 <- sbg/sesbg
		tstabrelvar1 <- sch/sesch
	}

	#########################################Result Matrix


# Result Matrix for two independent variables

if (analysis=="manifest") {

	univariate <- list()
	univariate[[1]] <- RR.1$univariate
	univariate[[2]] <- RR.2$univariate
	
	estimate <- c(saf,sbg,sag,sbf,sch,schs)
	standardized <- c(raf,rbg,rag,rbf,rch,rchs)
	se <- c(sesaf,sesbg,sesag,sesbf,sesch,seschs)
	t.value <- c(taf,tbg,tag,tbf,tch,tchs)
	bivariate <- data.frame(estimate, standardized, se, t.value)
	rownames(bivariate) <- bilabels_bb
	
	res <- list(univariate = univariate, bivariate = bivariate)
	
} else 
{
	# Result matrix for latent analysis 

	unstand<-c(stabpervar1,stabtarvar1,stabrelvar1,unstable1,stabapcov1,stabdycov1)
	stand<-  c(stabperr1, stabtarr1, stabrelr1, unstabler1,stabapcor1,stabdycor1)
	se<-     c(sestabpervar1,sestabtarvar1,sestabrelvar1,NA,NA,NA)
	tvalues<-c(tstabpervar1,tstabtarvar1,tstabrelvar1,NA,NA,NA)
	
	
	results <- data.frame(estimate=unstand,standardized=stand,se=se,t.value=tvalues)
	rownames(results) <- unilabels_b
	
	res <- list(univariate = results)
}

	class(res) <- "RR"
	return(res)
}



# Wrapper function: depending on parameters, different results are calculated:
# RR(liking_a) # single RR-group
# RR(liking_a, metaliking_a) # two variables (independent)
# RR(liking_a, liking_b, analysis=="latent") # two variables measure one latent construct
# RR(liking_a, liking_b, metaliking_a, metaliking_b, analysis=="latent") # two constructs, measured each with two variables
RR <- function(RRMatrix1, RRMatrix2=NULL, RRMatrix3=NULL, RRMatrix4=NULL, analysis="manifest") {

# as long as NA-handling is not implemented: set na.rm to FALSE
# (it will become a standard parameter soon ...)
na.rm=FALSE

if (!(analysis %in% c("latent", "manifest"))) stop("Parameter 'analysis' must either be 'latent' or 'manifest'. Calculations aborted.")
	
# depending on given parameters different results are calculated

#-----------------------------
#- One group

	if (is.null(RRMatrix2) & is.null(RRMatrix3) & is.null(RRMatrix4)) {
		if (analysis=="latent") print("Warning: analysis='latent' only is valid, when two different RRMatrices for one latent construct are given")
		
		res <- RR.univariate(RRMatrix1, na.rm)
		res$anal.type <- "Univariate analysis of one round robin variable"
		return(res)
	}
	
#-----------------------------
#- Two groups, independent or latent constructs

	if (is.null(RRMatrix3) & is.null(RRMatrix4)) {
		
		res <- RR.bivariate(RRMatrix1, RRMatrix2, analysis=analysis, na.rm)
		if (analysis=="latent") res$anal.type <- "Latent construct analysis of one construct measured by two round robin variables"
		if (analysis=="manifest") res$anal.type <- "Bivariate analysis of two variables, each measured by one round robin variable"
		return(res)
	}
	
	
#-----------------------------
#- four groups: two constructs measured with each two variables

	if (!is.null(RRMatrix2) & !is.null(RRMatrix3) & !is.null(RRMatrix4)) {
		
		if (analysis=="manifest") print("Warning: if four groups are provided, the function RR assumes two constructs measured with each two variables. Therefore parameter analysis automatically is set to 'latent'");
		
		# calculate latent effects for both constructs
		lat1.full <- RR.bivariate(RRMatrix1, RRMatrix2, analysis="latent", na.rm)$univariate
		lat2.full <- RR.bivariate(RRMatrix3, RRMatrix4, analysis="latent", na.rm)$univariate
		lat1 <- lat1.full$estimate
		lat2 <- lat2.full$estimate
		
		cross1 <- RR.bivariate(RRMatrix1, RRMatrix4, analysis="manifest", na.rm)$bivariate$estimate
		cross2 <- RR.bivariate(RRMatrix2, RRMatrix3, analysis="manifest", na.rm)$bivariate$estimate
		
		#Estimation of bivariate relations on construct level
		ppcov<-(cross1[1]+cross2[1])/2
		ppcor <- ppcov / sqrt(lat1[1]*lat2[1])
		
		ttcov <- (cross1[2]+cross2[2])/2
		ttcor <- ttcov / sqrt(lat1[2]*lat2[2])
		
		rrcov <- (cross1[5] + cross2[5])/2
		rrcor <- rrcov / sqrt (lat1[3]*lat2[3])
		
		ptcov <- (cross1[3]+cross2[3])/2
		ptcor <- ptcov / sqrt(lat1[1]*lat2[2])
		
		tpcov <- (cross1[4]+cross2[4])/2
		tpcor <- tpcov / sqrt(lat1[2]*lat2[1])
		
		rrscov <- (cross1[6] + cross2[6])/2
		rrscor <- rrscov / sqrt (lat1[3]*lat2[3])
		
		#Remark: Standard errors and t-values are not computed for bivariate relations on construct level

		#########################################Result Matrix

		estimate <- c(ppcov,ttcov,ptcov,tpcov,rrcov,rrscov)
		standardized <- c(ppcor,ttcor,ptcor,tpcor,rrcor,rrscor)

		bivariate <- data.frame(estimate, standardized, se=rep(NA, 6), t.value = rep(NA, 6))
		rownames(bivariate) <- bilabels_bb
		
		
		univariate <- list()
		univariate[[1]] <- lat1.full
		univariate[[2]] <- lat2.full
		
		grandres <- list(univariate = univariate, bivariate = bivariate)
		class(grandres) <- "RR"
		grandres$anal.type <- "Bivariate analysis of two constructs, each measured by two round robin variables"
		
		return(grandres)		
	}
	
	# if no condition above had a hit: error
	stop("Error: parameters are wrongly specified!")
	
}

# Here the default print method for RR-objects gets overwritten, so that 
# the information in the RR-class is displayed in a convenient way
print.RR <- function(x, ..., measure1="behavior", measure2="behavior") {
	print(paste("Round-Robin object ('RR'), calculated by Triple-R\n-->", x$anal.type, ":\n\n"))
	
	# bivariate case
	if (length(x$univariate) == 2) {
		
		uni <- x$univariate
		bi <- x$bivariate
		if (measure1 == "behavior" & measure2 == "behavior") {
			rownames(uni[[1]]) <- unilabels_b
			rownames(uni[[2]]) <- unilabels_b
			rownames(bi) <- bilabels_bb
		}
   if (measure1 == "behavior" & measure2 == "perception") {
			rownames(uni[[1]]) <- unilabels_b
			rownames(uni[[2]]) <- unilabels_p
			rownames(bi) <- bilabels_bp
		}
   if (measure1 == "perception" & measure2 == "behavior") {
			rownames(uni[[1]]) <- unilabels_p
			rownames(uni[[2]]) <- unilabels_b
			rownames(bi) <- bilabels_pb
		}
   if (measure1 == "perception" & measure2 == "perception") {
			rownames(uni[[1]]) <- unilabels_p
			rownames(uni[[2]]) <- unilabels_p
			rownames(bi) <- bilabels_pp
		}
		if (measure1 == "perception" & measure2 == "metaperception") {
			rownames(uni[[1]]) <- unilabels_b_meta1
			rownames(uni[[2]]) <- unilabels_b_meta2
			rownames(bi) <- bilabels_meta
		}
		print("Univariate analyses, variable 1:\n\n")
		print(uni[[1]])
		print("\n\nUnivariate analyses, variable 2:\n\n")
		print(uni[[2]])
		print("\n\nBivariate analyses:\n\n")
		print(bi)
		
	} else
	
	# univariate case
	{
		uni <- x$univariate
		if (measure1 == "behavior") rownames(uni) <- unilabels_b
		if (measure1 == "perception") rownames(uni) <- unilabels_p
		if (measure1 == "metaperception") {
			print("Warning: the current RR-object only consists of one RRMatrix. Labels for metaperception are only provided when two RRMatrices are calculated.\n\n")
			rownames(uni) <- unilabels_b
		}
		print(uni)
		
	}
	
}