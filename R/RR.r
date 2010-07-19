

#------------------------------------
#-- Skinning variables
#------------------------------------


localOptions <- new.env(parent=globalenv())
localOptions$suffixes <- c(".a", ".p", ".s")
localOptions$style <- "behavior"

role <- list()
role$behavior <- c("Actor", "Partner", "Relationship")
role$perception <- c("Perceiver", "Target", "Relationship")

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


# set options for printing reults etc.
# style = c("behavior", "perception")
RR.style <- function(style="behavior", suffixes=NA) {
	localOptions$style <- style <- match.arg(style, c("behavior", "perception"))
	
	if (is.na(suffixes)) {
		if (style=="behavior") {
			localOptions$suffixes <- c(".a", ".p", ".s")
		} else 
		if (style=="perception") {
			localOptions$suffixes <- c(".p", ".t", ".s")
		}
	} else {
		localOptions$suffixes <- suffixes
	}
}


# corrects values < -1 to -1 and values > 1 to 1
clamp <- function(...) {
	x <- c(...)
	x[x < -1] <- -1
	x[x >  1] <-  1
	return(x)
}


# function takes data in the "long" format and returns a list with quadratic
# round robin matrices for each group
long2matrix <- function(formule, data, verbose=TRUE, reduce=TRUE, skip3=FALSE, g.id=NULL) {
	
	# parse formula
	#remove spaces from formula
	
	
	lhs <- strsplit(gsub(" ","",as.character(formule)[2], fixed=TRUE), "+", fixed=TRUE)[[1]]
	rhs <- strsplit(gsub(" ","",as.character(formule)[3], fixed=TRUE),"\\*|\\|", perl=TRUE)[[1]]
	
	var.id <- lhs
	actor.id <- rhs[1]
	partner.id <- rhs[2]
	if (length(rhs)>=3) {group.id <- rhs[3]} else {group.id=NULL}
	
	# What are the group ids?
	# if only one group, add group variable
	
	if (!is.null(group.id)) {if (is.na(group.id)) {group.id <- NULL}}
	if (!is.null(group.id)) {
			group.ids <- names(table(data[,group.id]))
	} else {
		if (!is.null(g.id)) {
			group.id <- g.id
			group.ids <- names(table(data[,group.id]))
		} else {
			group.ids <- "1"
			group.id <- "group.id"
			data$group.id <- "1"
		}
	}
	
	# reduce data.frame to relevant variables
	data <- data[, colnames(data) %in% c(actor.id, partner.id, var.id, group.id)]
	
	res <- list()
	
	for (g in group.ids) {
		
		#print(paste("Processing group",g))
		block <- data[data[,group.id]==g,]
		
		# reduce ids to factor levels which are actually present
		block[,actor.id] <- factor(block[,actor.id])
		block[,partner.id] <- factor(block[,partner.id])
		block[,group.id] <- factor(block[,group.id])
						
		# get names of participants which served both as actors and observers
		if (reduce==TRUE) {
			
			# Schnittmenge aus actors und partners herausfinden
			p <- intersect(levels(as.factor(block[,actor.id])), levels(as.factor(block[,partner.id])))
			block.clean <- block[block[,actor.id] %in% p & block[,partner.id] %in% p, which(colnames(block) != group.id)]
			t1 <- table(block.clean$actor.id, block.clean$partner.id)
			delrow <- rownames(t1)[apply(t1, 1, sum, na.rm=TRUE)==0]
			delcol <- rownames(t1)[apply(t1, 2, sum, na.rm=TRUE)==0]
			p2 <- setdiff(p, union(delrow, delcol))
			
		} else {
			p2 <- union(levels(as.factor(block[,actor.id])), levels(as.factor(block[,partner.id])))
		}
			
			
		if (length(p2) <= 1) {
			warning(paste("Warning: The provided data of group",g,"are not in round robin format!"));
			next();
		}
			
		block.clean <- block[block[,actor.id] %in% p2 & block[,partner.id] %in% p2, which(colnames(block) != group.id)]




		
		if (verbose==TRUE & nrow(block.clean)<nrow(block)) {
			#warning(paste("Warning: Some participants in group",g,"are only actors or only partners. They are removed from the round robin matrix."))
		}
		
		# finally: construct the quadratic matrix
		f1 <- as.formula(paste(actor.id,"~",partner.id))
		
		box <- as.matrix(cast(block.clean, f1, value=var.id, fill=NA))
		
		if (reduce==FALSE) {
			colmiss <- setdiff(levels(as.factor(block[,actor.id])), levels(as.factor(block[,partner.id])))
			rowmiss <- setdiff(levels(as.factor(block[,partner.id])), levels(as.factor(block[,actor.id])))
			if (length(colmiss)>0) box <- cbind(box, matrix(NA, nrow=nrow(box), ncol=length(colmiss), dimnames=list(NULL, colmiss)))
			if (length(rowmiss)>0) box <- rbind(box, matrix(NA, ncol=ncol(box), nrow=length(rowmiss), dimnames=list(rowmiss, NULL)))
			
			box <- box[order(rownames(box)), order(colnames(box))]
		}
		
		# extract self ratings if present
		self <- diag(box)
		diag(box) <- NA
		
		# Voyeure entfernen (nur actors oder nur partners) - iterativ
		if (reduce) {
			repeat {
				del.row <- apply(box, 1, function(x) sum(is.na(x)) >= length(x))
				del.col <- apply(box, 2, function(x) sum(is.na(x)) >= length(x))
				dels <- del.row | del.col
				if (sum(dels==TRUE) == 0) {break();}
				box <- box[!dels,!dels]
				self <- self[!dels]
			}
		}
		
		
		if (!skip3 | nrow(box)>3) {
			res[[g]] <- box
			attr(res[[g]], "group.id") <- g
			attr(res[[g]], "varname") <- var.id
			if (any(!is.na(self))) {
				attr(res[[g]], "self.ratings") <- self
			}
		}
		if (skip3 & nrow(box)<=3) {
			warning(paste("WARNING: group",g,"has 3 or fewer subjects. For calculation of SRM variables minimum group size is 4 - the group is excluded from the analyses"));
		}
	
	}
	
	
	if (length(res)==0) {return()} 
	else {return(res)}
}



clearLongData <- function(formule, data) {
	ll1 <- long2matrix(formule, data, reduce=TRUE)
	
	lhs <- strsplit(gsub(" ","",as.character(formule)[2], fixed=TRUE), "+", fixed=TRUE)[[1]]
	rhs <- strsplit(gsub(" ","",as.character(formule)[3], fixed=TRUE),"\\*|\\|", perl=TRUE)[[1]]
	
	var.id <- lhs
	actor.id <- rhs[1]
	partner.id <- rhs[2]
	if (length(rhs)>=3) {group.id <- rhs[3]} else {group.id="group.id"}
	
	
	
	ll2 <- ldply(ll1, function(x) {
		matrix2long(x, new.ids=FALSE, var.id=var.id)
	})
	colnames(ll2)[1:3] <- c(group.id, actor.id, partner.id)

	return(ll2)
}



# function takes data in matrix format and turns them into long format
matrix2long <- function(M, new.ids=TRUE, var.id="value") {
	
	M <- as.matrix(M)
	if (new.ids) {
		rownames(M) <- colnames(M) <- seq(1, nrow(M))
	}
	m1 <- melt(as.matrix(M))
	colnames(m1) <- c("actor.id", "partner.id", var.id)
	
	return(m1)
}




# calculates Actor-, Partner- and Relationship-Effects from a single RR-Matrix
RR.effects <- function(RRMatrix, name=NA) {

	if (!is.null(attr(RRMatrix, "varname"))) name <- attr(RRMatrix, "varname")

	 mip <- apply(RRMatrix, 1, mean, na.rm=TRUE)
	 mpj <- apply(RRMatrix, 2, mean, na.rm=TRUE)
	 mpp <- mean(as.matrix(RRMatrix), na.rm=TRUE)  # grand mean

	 n <- length (mip)
	 a <- ((n-1)^2)/(n*(n-2))*mip + (n-1)/(n*(n-2))*mpj - (n-1)/(n-2)*mpp #actor effects
	 b <- ((n-1)^2)/(n*(n-2))*mpj + (n-1)/(n*(n-2))*mip - (n-1)/(n-2)*mpp #partner effects

	 am <- matrix(a, nrow(RRMatrix), ncol(RRMatrix))
	 bm <- t(matrix(b, nrow(RRMatrix), ncol(RRMatrix)))
	 c <- as.matrix(RRMatrix - am - bm - mpp) # relationship effect
	rownames(c) <- colnames(c) <- rownames(RRMatrix)
	
	# return effects also in long format
	if (!is.null(attr(RRMatrix, "self.ratings"))) {
		eff <- data.frame(id = rownames(RRMatrix), actor=a, partner=b, self=attr(RRMatrix, "self.ratings")-mpp)
		if (!is.null(name)) {colnames(eff)[2:4] <- paste(name, localOptions$suffixes, sep="")}
	} else {
		eff <- data.frame(id = rownames(RRMatrix), actor=a, partner=b)
		if (!is.null(name)) {colnames(eff)[2:3] <- paste(name, localOptions$suffixes[1:2], sep="")}
	}
	
	
	
	effRel <- melt(c)
	effRel <- effRel[apply(effRel, 1, function(x) {x[1] != x[2]}),]
	colnames(effRel) <- c("actor.id", "partner.id","relationship")
	effRel$dyad <- apply(effRel, 1, function(x) paste(as.character(min(x[1], x[2])), as.character(max(x[1], x[2])), sep="_"))
	effRel <- effRel[,c(1,2,4,3)]
	effRel <- effRel[order(effRel$dyad),]
	
	eff.gm <- eff
	if (!is.null(attr(RRMatrix, "self.ratings"))) {
		eff.gm[,2:4] <- eff.gm[,2:4]+mpp
	} else {
		eff.gm[,2:3] <- eff.gm[,2:3]+mpp
	}
	
	if (!is.null(attr(RRMatrix, "self.ratings"))) {
		return(list(actor = a, partner = b, relationship = c, eff=eff, effRel=effRel, eff.gm=eff.gm, self=attr(RRMatrix, "self.ratings")-mpp))
	} else {
		return(list(actor = a, partner = b, relationship = c, eff=eff, effRel=effRel, eff.gm=eff.gm))
		}
}



# calculates variance components from a single RR-Matrix
# na.rm = TRUE / FALSE / "impute" (<- not implemented yet)
RR.univariate <- function(RRMatrix, na.rm=FALSE, verbose=TRUE, corr.fac=NA) {
	
	if (is.null(RRMatrix)) return();
	
	if (nrow(RRMatrix)<4) {
		warning(paste("WARNING: group",attr(RRMatrix, "group.id"),"has 3 or fewer subjects. For calculation of SRM variables minimum group size is 4."));
		return();
	}
	
	
	# emit some warnings about missings if there are NAs outside the diagonale
	if ((sum(is.na(RRMatrix)) > nrow(RRMatrix)) & na.rm==FALSE)
		stop("There are NAs outside the diagonale. Calculations are aborted.")
		
	if ((sum(is.na(RRMatrix)) > nrow(RRMatrix)) & na.rm==TRUE) {
		#warning("Note: There are NAs outside the diagonale. Degrees of freedom in Kenny's (1994) formula are adjusted for missings. THIS IS EXPERIMENTAL AND NOT THOROUGHLY TESTED. Maybe you should think about imputation procedures to obtain a complete matrix.")
	}
	
	eff <- RR.effects(RRMatrix, name=attr(RRMatrix, "varname"))
	n <- nrow(RRMatrix)
	
	A <- sum(eff$actor^2)/(n-1)
	B <- sum(eff$partner^2)/(n-1)
	C <- sum(eff$actor*eff$partner)/(n-1)
	e <- 0.5*(eff$relationship + t(eff$relationship))
	d <- eff$relationship - t(eff$relationship)
	
	if (na.rm==TRUE) {
		
		if (is.na(corr.fac)) {
			corr.fac <- (n*(n-1)) / (n*(n-1) - sum(is.na(e)) + n)
		} else {
			corr.fac <- eval(parse(text=corr.fac))
		}
		
		D <- (sum(e^2, na.rm=TRUE) * corr.fac)  / (((n-1)*(n-2)/2)-1)
		E <- ((sum(d^2,na.rm=TRUE)/2)  * corr.fac) / ((n-1)*(n-2))
	} else {
		D <- sum(e^2, na.rm=TRUE) / (((n-1)*(n-2)/2)-1)
		E <- (sum(d^2,na.rm=TRUE)/2) / ((n-1)*(n-2))
	}
	
	
	scc <- (D+E)/2 #relationship variance
	sccs <- (D-E)/2 #relationship covariance
	sab <- C - (sccs*(n-1))/(n*(n-2)) - scc/(n*(n-2)) #actor-partner covariance
	saa <- A - (scc*(n-1))/(n*(n-2)) - sccs/(n*(n-2)) #actor variance
	sbb <- B - (scc*(n-1))/(n*(n-2)) - sccs/(n*(n-2)) #partner variance
	
	saa2 <- ifelse(saa>=0, saa, NaN)
	sbb2 <- ifelse(sbb>=0, sbb, NaN)
	scc2 <- ifelse(scc>=0, scc, NaN)
	
	raa <- saa2/sum(saa2,sbb2,scc2,na.rm=TRUE) #standardized actor variance
	rbb <- sbb2/sum(saa2,sbb2,scc2,na.rm=TRUE) #standardized partner variance
	rcc <- scc2/sum(saa2,sbb2,scc2,na.rm=TRUE) #standardized relationship variance
	rab <- ifelse(saa>0 & sbb>0,sab/sqrt(saa*sbb),NaN) #actor-partner correlation
	rccs <- sccs/scc2 #relationship correlation
	
	
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


	# error variance is NA if only one group is present
	estimate <- c(saa,sbb,scc,NA,sab,sccs)
	standardized <- clamp(raa,rbb,rcc,NA,rab,rccs)
	
	se <- c(ifelse(estimate[1:3]>=0,c(sesaa,sesbb,sescc),NaN),NA,sesab,sesccs)
	t.value <- estimate/se
	p.value <- dt(t.value, n-1)
	# Kovarianzen werden zweiseitig getestet:
	p.value[4:5] <- p.value[4:5]*2
	
	
	# calculate reliability for actor and partner effects
	rel.a <- saa / (saa + scc*(n-1)/(n*(n-2)) + sccs/(n*(n-2)))
	if (saa < 0) rel.a <- NaN
	rel.b <- sbb / (sbb + scc*(n-1)/(n*(n-2)) + sccs/(n*(n-2)))
	if (sbb < 0) rel.b <- NaN
	
	attr(eff$eff[,2], "reliability") <- rel.a
	attr(eff$eff[,3], "reliability") <- rel.b
	
	# join everything in one dataframe
	univariate <- data.frame(type=unilabels_b, estimate, standardized, se, t.value, p.value)
	
	# if one variance component is below zero: erase covariances
	# erase indices for negative variances
	univariate[1:3,][univariate$estimate[1:3]<0,3:6] <- NaN
	if (saa <= 0 | sbb <= 0) {univariate[5,3:6] <- NaN}
	if (scc <= 0) {univariate[6,3:6] <- NaN}


	res <- list(effects = eff$eff, effectsRel = eff$effRel, effects.gm = eff$eff.gm, varComp = univariate, relMat.av=e, relMat.diff=d, group.size=n, latent=FALSE, anal.type="Univariate analysis of one round robin variable")
	class(res) <- "RRuni"
	attr(res, "group.size") <- n
	attr(res, "varname") <- attr(RRMatrix, "varname")
	return(res)
}



# combines two RR-matrices, depending on parameter 'latent'
# latent = TRUE: both matrices are treated as two measures for one underlying construct
# latent = FALSE: both matrices are treated as independent variables
# noCorrection = TRUE: even if univariate estimates are negative, bivariate covariances are NOT set to NA (this is necessary, when the manifest bivariat results are transferred into the bivariate latent analysis, see TAG1)

RR.bivariate <- function(RRMatrix1, RRMatrix2, analysis="manifest", na.rm=FALSE, verbose=TRUE, noCorrection=FALSE) {
	
	if (!(analysis %in% c("latent", "manifest"))) stop("Parameter 'analysis' must either be 'latent' or 'manifest'. Calculations aborted.")
	
	RR.1 <- RR.univariate(RRMatrix1, na.rm, verbose)
	RR.2 <- RR.univariate(RRMatrix2, na.rm, verbose)	
	varComp.1 <- RR.1$varComp$estimate
	varComp.2 <- RR.2$varComp$estimate
	n <- nrow(RRMatrix1)

	#Bivariate Relationships

	A <- sum(RR.1$effects[,2]*RR.2$effects[,2])/(n-1)
	B <- sum(RR.1$effects[,2]*RR.2$effects[,3])/(n-1)
	C <- sum(RR.1$effects[,3]*RR.2$effects[,2])/(n-1)
	D <- sum(RR.1$effects[,3]*RR.2$effects[,3])/(n-1)
	E <- sum(RR.1$relMat.av*RR.2$relMat.av,na.rm=TRUE)/(((n-1)*(n-2)/2)-1)
	F <- (sum(RR.1$relMat.diff*RR.2$relMat.diff,na.rm=TRUE)/2)/((n-1)*(n-2))
	sch <- (E+F)/2 #intrapersonal relationship covariance
	schs <- (E-F)/2 #interpersonal relationship covariance
	saf <- A - (sch*(n-1))/(n*(n-2)) - schs/(n*(n-2)) #actor-actor covariance
	sag <- B - (schs*(n-1))/(n*(n-2)) - sch/(n*(n-2)) #actor-partner covariance
	sbf <- C - (schs*(n-1))/(n*(n-2)) - sch/(n*(n-2)) #partner-actor covariance
	sbg <- D - (sch*(n-1))/(n*(n-2)) - schs/(n*(n-2)) #partner-partner covariance
	
	
	# standardized covariances (=correlations), bivariate case
	#standardized <- clamp(raf,rbg,rag,rbf,rch,rchs)
	w <- getOption("warn")
	options(warn=-1)
		raf <-  saf/(sqrt(varComp.1[1])*sqrt(varComp.2[1])) # bivariate correlations
		rbg <-  sbg/(sqrt(varComp.1[2])*sqrt(varComp.2[2]))
		rag <-  sag/(sqrt(varComp.1[1])*sqrt(varComp.2[2]))
		rbf <-  sbf/(sqrt(varComp.1[2])*sqrt(varComp.2[1]))
		rch <-  sch/(sqrt(varComp.1[3])*sqrt(varComp.2[3]))
		rchs <- schs/(sqrt(varComp.1[3])*sqrt(varComp.2[3]))
	options(warn=w)
	
	
	if (analysis=="latent") {
		stabpervar1 <- saf
		stabtarvar1 <- sbg
		stabrelvar1 <- sch
		
    	stabapcov1 <- (sag + sbf)/2		
    	stabdycov1 <- schs
		unstabper1 <- (varComp.1[1] + varComp.2[1])/2 - saf
		unstabtar1 <- (varComp.1[2] + varComp.2[2])/2 - sbg
		unstabrel1 <- (varComp.1[3] + varComp.2[3]) / 2 - sch
		
		
		saf2 <- max(saf, 0)
		sbg2 <- max(sbg, 0)
		sch2 <- max(sch, 0)
		
		stable1 <- saf2 + sbg2 + sch2
		unstable1 <- max(unstabper1, 0) + max(unstabtar1, 0) + max (unstabrel1, 0) 
		unstable.raw <- sum(unstabper1, unstabtar1, unstabrel1)
		stabler1 <- stable1 / (stable1 + unstable1)
		unstabler1 <- unstable1 / (stable1 + unstable1)
		stabperr1 <- saf2/(stable1 + unstable1)
		stabtarr1 <- sbg2/(stable1+unstable1)
		stabrelr1 <- sch2/(stable1+unstable1)
		stabdycor1 <- ifelse(sch2>0, stabdycov1/sch2, NaN)
		stabapcor1 <- ifelse(saf>0 & sbg>0, stabapcov1 / sqrt(saf*sbg), NaN)
	}
	

	# Standard errors (se) und t-values (t) of bivariate srm-parameters

	coef1 <- n^11-14*n^10+89*n^9-342*n^8+872*n^7-1505*n^6+1698*n^5-1063*n^4+116*n^3+292*n^2-224*n+64
	coef2 <- n^10-12*n^9+66*n^8-227*n^7+534*n^6-857*n^5+883*n^4-416*n^3-148*n^2+224*n-64
	coef3 <- n^10-11*n^9+48*n^8-93*n^7-2*n^6+388*n^5-763*n^4+572*n^3+4*n^2-224*n+64
	coef4 <- n^11-16*n^10+117*n^9-520*n^8+1540*n^7-3083*n^6+3970*n^5-2689*n^4-4*n^3+1212*n^2-544*n-64
	coef5 <- n^10-11*n^9+42*n^8-33*n^7-258*n^6+976*n^5-1453*n^4+788*n^3+348*n^2-544*n-64
	coef6 <- 2*(n^10-14*n^9+92*n^8-383*n^7+1074*n^6-1963*n^5+2101*n^4-752*n^3-780*n^2+544*n+64)
	coef7 <- (n-3)*n*(n^6-9*n^5+35*n^4-75*n^3+76*n^2-12*n-48)

	suppressWarnings(
		sesaf <- sqrt(((n-1)*varComp.1[1]*varComp.2[1]+(n-3)*saf^2)/((n-2)*(n+1))
		         + ((n-1)^2*(varComp.1[1]*varComp.2[3]+varComp.1[3]*varComp.2[1])+(n-1)*(varComp.1[1]*varComp.2[5]+varComp.1[5]*varComp.2[1])+2*(n-3)*saf*((n-1)*sch+schs))/((n-2)^2*n*(n+1))
		         + (coef1*varComp.1[3]*varComp.2[3]+coef2*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+coef3*varComp.1[5]*varComp.2[5]+coef4*sch^2+coef5*schs^2+coef6*sch*schs)/((n-2)^3*n^2*(n+1)*coef7))
		)

	suppressWarnings(
		sesbg <- sqrt(((n-1)*varComp.1[2]*varComp.2[2]+(n-3)*sbg^2)/((n-2)*(n+1))
		         + ((n-1)^2*(varComp.1[2]*varComp.2[3]+varComp.1[3]*varComp.2[2])+(n-1)*(varComp.1[2]*varComp.2[5]+varComp.1[5]*varComp.2[2])+2*(n-3)*sbg*((n-1)*sch+schs))/((n-2)^2*n*(n+1))
		         + (coef1*varComp.1[3]*varComp.2[3]+coef2*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+coef3*varComp.1[5]*varComp.2[5]+coef4*sch^2+coef5*schs^2+coef6*sch*schs)/((n-2)^3*n^2*(n+1)*coef7))
		)
		
		
	suppressWarnings(
	sesch <- sqrt(((n^6-9*n^5+32*n^4-57*n^3+43*n^2+6*n-8)*(varComp.1[3]*varComp.2[3]+varComp.1[5]*varComp.2[5])+2*(n^4-6*n^3+3*n^2+18*n-8)*sch*schs)/coef7
	         + ((n^4-6*n^3+11*n^2-6*n+8)*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+(n^6-9*n^5+28*n^4-33*n^3-9*n^2+54*n+8)*(sch^2 + schs^2))/coef7)
			)
			
	suppressWarnings(
	sesag <- sqrt(((n-1)*varComp.1[1]*varComp.2[2]+(n-3)*sag^2)/((n-2)*(n+1))
	         + ((n-1)^2*(varComp.1[1]*varComp.2[3]+varComp.1[3]*varComp.2[2])+(n-1)*(varComp.1[1]*varComp.2[5]+varComp.1[5]*varComp.2[2])+2*(n-3)*sag*((n-1)*schs+sch))/((n-2)^2*n*(n+1))
	         + (coef1*varComp.1[3]*varComp.2[3]+coef2*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+coef3*varComp.1[5]*varComp.2[5]+coef4*schs^2+coef5*sch^2+coef6*sch*schs)/((n-2)^3*n^2*(n+1)*coef7))
	)

	suppressWarnings(
	sesbf <- sqrt(((n-1)*varComp.1[2]*varComp.2[1]+(n-3)*sbf^2)/((n-2)*(n+1))
	         + ((n-1)^2*(varComp.1[2]*varComp.2[3]+varComp.1[3]*varComp.2[1])+(n-1)*(varComp.1[2]*varComp.2[5]+varComp.1[5]*varComp.2[1])+2*(n-3)*sbf*((n-1)*schs+sch))/((n-2)^2*n*(n+1))
	         + (coef1*varComp.1[3]*varComp.2[3]+coef2*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+coef3*varComp.1[5]*varComp.2[5]+coef4*schs^2+coef5*sch^2+coef6*sch*schs)/((n-2)^3*n^2*(n+1)*coef7))
	)


	suppressWarnings(
	seschs <- sqrt(((n^6-9*n^5+32*n^4-57*n^3+43*n^2+6*n-8)*(varComp.1[3]*varComp.2[3]+varComp.1[5]*varComp.2[5])+2*(n^4-6*n^3+3*n^2+18*n-8)*sch*schs)/coef7
	         + ((n^4-6*n^3+11*n^2-6*n+8)*(varComp.1[3]*varComp.2[5]+varComp.1[5]*varComp.2[3])+(n^6-9*n^5+28*n^4-33*n^3-9*n^2+54*n+8)*(sch^2 + schs^2))/coef7)
	)


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

		tstabpervar1 <- ifelse(saf>=0, saf/sesaf, NaN)
		tstabtarvar1 <- ifelse(sbg>=0, sbg/sesbg, NaN)
		tstabrelvar1 <- ifelse(sch>=0, sch/sesch, NaN)
	}

	#########################################Result Matrix


# Result Matrix for two independent variables

if (analysis=="manifest") {

	univariate <- list()
	univariate[[1]] <- RR.1
	univariate[[2]] <- RR.2
	
	estimate <- c(saf,sbg,sag,sbf,sch,schs)
	standardized <- clamp(raf,rbg,rag,rbf,rch,rchs)
	
	se <- c(sesaf,sesbg,sesag,sesbf,sesch,seschs)
	t.value <- c(taf,tbg,tag,tbf,tch,tchs)
	pvalues <- dt(t.value, n-1)*2 	# alles Kovarianzen, daher zweiseitig testen!
	bivariate <- data.frame(type=bilabels_bb, estimate, standardized, se, t.value, p.value=pvalues)
	
	if (noCorrection==FALSE) {
		# erase covariances if one variance component is < 0
		if (RR.1$varComp[1,2] <= 0) bivariate[c(1,3),3:6] <- NaN
		if (RR.1$varComp[2,2] <= 0) bivariate[c(2,4),3:6] <- NaN
		if (RR.2$varComp[1,2] <= 0) bivariate[c(1,4),3:6] <- NaN
		if (RR.2$varComp[2,2] <= 0) bivariate[c(2,3),3:6] <- NaN
		if (RR.1$varComp[3,2] <= 0) bivariate[c(5,6),3:6] <- NaN
		if (RR.2$varComp[3,2] <= 0) bivariate[c(5,6),3:6] <- NaN
	}	
	
	res <- list(univariate = univariate, bivariate = bivariate, latent=FALSE, anal.type="Bivariate analysis of two variables, each measured by one round robin variable")
	attr(res, "group.size") <- n
	class(res) <- "RRbi"
	
} else 
{
	# Result matrix for latent analysis 

	unstand <- c(stabpervar1,stabtarvar1,stabrelvar1,unstable1,stabapcov1,stabdycov1)
	stand <- clamp(stabperr1, stabtarr1, stabrelr1, unstabler1,stabapcor1,stabdycor1)
	stand[is.infinite(stand)] <- NaN
	
	se <- c(sestabpervar1,sestabtarvar1,sestabrelvar1,NA,NA,NA)
	tvalues<-c(tstabpervar1,tstabtarvar1,tstabrelvar1,NA,NA,NA)
	pvalues <- dt(tvalues, n-1)
	pvalues[4:5] <- pvalues[4:5]*2
	
	results <- data.frame(type=unilabels_b, estimate=unstand,standardized=stand,se=se,t.value=tvalues, p.value=pvalues)
	
	# erase indices for negative variances
	results[1:3,][results$estimate[1:3]<0,3:6] <- NaN
	
	#-------------------------------
	# calculate reliability for actor and partner effects
	
	unstabdycov1 <- (varComp.1[5] + varComp.2[5]) / 2 - schs
	
	r <- 2 # r = number of replications - in our case, it's always 2
	rel.a <- stabpervar1 / ((stabpervar1 + (unstabper1/r)) + (stabrelvar1+(unstabrel1/r))*(n-1)/(n*(n-2)) + (stabdycov1+(unstabdycov1/r))/(n*(n-2)))
	if (stabpervar1 < 0) rel.a <- NaN
	
	rel.p <- stabtarvar1 / ((stabtarvar1 + (unstabtar1/r)) + (stabrelvar1+(unstabrel1/r))*(n-1)/(n*(n-2)) + (stabdycov1+(unstabdycov1/r))/(n*(n-2)))
	if (stabtarvar1 < 0) rel.p <- NaN
	
	rel.r <- stabrelvar1 / (stabrelvar1+(unstabrel1/r))
	if (stabrelvar1 < 0) rel.r <- NaN
	
	#-------------------------------
	
	
	eff2 <- merge(RR.1$effects, RR.2$effects, by=c("id"))
	
	eff2$actor <- apply(eff2[,grep(localOptions$suffixes[1], colnames(eff2))], 1, mean, na.rm=TRUE)
	eff2$partner <- apply(eff2[,grep(localOptions$suffixes[2], colnames(eff2))], 1, mean, na.rm=TRUE)
	
	# self ratings vorhanden?
	if (ncol(RR.1$effects)==4) {
		eff2$self <- apply(eff2[,grep(localOptions$suffixes[3], colnames(eff2))], 1, mean, na.rm=TRUE)
	}
	
	effRel2 <- merge(RR.1$effectsRel, RR.2$effectsRel, by=c("actor.id", "partner.id"))
	effRel2$relationship <- apply(effRel2[,c("relationship.x", "relationship.y")], 1, mean, na.rm=TRUE)
	
	if (ncol(RR.1$effects)==4) {
		eff <- eff2[,c("id", "actor", "partner", "self")]
	} else {
		eff <- eff2[,c("id", "actor", "partner")]
	}
	colnames(eff) <- colnames(RR.1$effects)
		
	effRel <- effRel2[,c("actor.id", "partner.id", "relationship")]
	
	attr(eff[,2], "reliability") <- rel.a
	attr(eff[,3], "reliability") <- rel.p
	attr(effRel$relationship, "reliability") <- rel.r
	
	res <- list(effects = eff, effectsRel=effRel, varComp=results, unstabdycov1=unstabdycov1, unstabper1=unstabper1, unstabtar1=unstabtar1, unstabrel1=unstabrel1, unstable.raw=unstable.raw, latent=TRUE, anal.type="Latent construct analysis of one construct measured by two round robin variables")
	attr(res, "group.size") <- n
	attr(res, "varname") <- paste(attr(RR.1, "varname"), attr(RR.2, "varname"), sep="/")
	class(res) <- "RRuni"
}

	
	return(res)
}




# helper function
ifg <- function(g) {
	if (is.null(g)) {
		return("");
	} else {
		return(paste("|",g));
	}
}


# Wrapper function: depending on parameters, different results are calculated:
RR <- function(formula, data, na.rm=FALSE, verbose=TRUE, g.id=NULL) {

# set default
analysis <- "manifest"
RRMatrix2 <- RRMatrix3 <- RRMatrix4 <- NULL

# transform long format (formula) to quadratic matrices
#print("Long format is converted in quadratic matrices ...")
if (is.null(data)) stop("If a formula is specified, an explicit data object has to be provided!");

#remove spaces from formula
f1 <- formula
lhs <- strsplit(gsub(" ","",as.character(f1)[2], fixed=TRUE), "+", fixed=TRUE)[[1]]
rhs <- strsplit(gsub(" ","",as.character(f1)[3], fixed=TRUE),"\\*|\\|", perl=TRUE)[[1]]

actor.id <- rhs[1]
partner.id <- rhs[2]
if (length(rhs)>=3) {group.id <- rhs[3]} else {group.id=NULL}


# if a grouping factor is provided: forward to RR.multi
if (!is.null(group.id)) {
	return(RR.multi(f1, data=data, na.rm=na.rm, verbose=verbose));
}



# univariater Fall:
if (length(lhs)==1) {
	lhs1 <- strsplit(lhs, "/")[[1]]
	
	# manifester vs. latenter Fall
	if (length(lhs1)==1) {
		RRMatrix1 <- long2matrix(formula(paste(lhs1,"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		analysis <- "manifest"
	} else if (length(lhs1)==2) {
		RRMatrix1 <- long2matrix(formula(paste(lhs1[1],"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		RRMatrix2 <- long2matrix(formula(paste(lhs1[2],"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		analysis <- "latent"
	}
	
} else 
# bivariater Fall
if (length(lhs)==2) {
	
	lhs1 <- strsplit(lhs[1], "/")[[1]]
	
	# manifester vs. latenter Fall
	if (length(lhs1)==1) {
		RRMatrix1 <- long2matrix(formula(paste(lhs[1],"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		RRMatrix2 <- long2matrix(formula(paste(lhs[2],"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		analysis <- "manifest"
	} else if (length(lhs1)==2) {
		lhs2 <- strsplit(lhs[2], "/")[[1]]
		RRMatrix1 <- long2matrix(formula(paste(lhs1[1],"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		RRMatrix2 <- long2matrix(formula(paste(lhs1[2],"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		RRMatrix3 <- long2matrix(formula(paste(lhs2[1],"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		RRMatrix4 <- long2matrix(formula(paste(lhs2[2],"~",actor.id,"*",partner.id,ifg(group.id))), data, verbose=verbose, skip3=TRUE, g.id=g.id)[[1]]
		analysis <- "latent"
	}
} else {stop("Error: Unknown term in formula.")}
	


	
# depending on given parameters different results are calculated

#-----------------------------
#- One group

	if (is.null(RRMatrix2) & is.null(RRMatrix3) & is.null(RRMatrix4)) {
		if (analysis=="latent") warning("Warning: analysis='latent' only is valid, when two different RRMatrices for one latent construct are given")
		
		res <- RR.univariate(RRMatrix1, na.rm, verbose)
		return(res)
	}
	
#-----------------------------
#- Two groups, independent or latent constructs

	if (is.null(RRMatrix3) & is.null(RRMatrix4)) {
		
		res <- RR.bivariate(RRMatrix1, RRMatrix2, analysis=analysis, na.rm=na.rm, verbose=verbose)
		return(res)
	}
	
	
#-----------------------------
#- four groups: two constructs measured with each two variables

	if (!is.null(RRMatrix2) & !is.null(RRMatrix3) & !is.null(RRMatrix4)) {
		
		if (analysis=="manifest") warning("Warning: if four groups are provided, the function RR assumes two constructs measured with each two variables. Therefore parameter analysis automatically is set to 'latent'");
		
		# calculate latent effects for both constructs
		lat1.full <- RR.bivariate(RRMatrix1, RRMatrix2, analysis="latent", na.rm=na.rm, verbose=verbose)
		lat2.full <- RR.bivariate(RRMatrix3, RRMatrix4, analysis="latent", na.rm=na.rm, verbose=verbose)
		lat1 <- lat1.full$varComp$estimate
		lat2 <- lat2.full$varComp$estimate
				
		
		# calculate new raw data: mean of both indicators; a manifest bivariate analysis is conducted on the mean variable
		# --> all calculations are correct, except the standardization --> this has to be done on the latent data
		RR12 <- (RRMatrix1+RRMatrix2)/2
		RR34 <- (RRMatrix3+RRMatrix4)/2
		
		#TAG1 <-- this is a bookmark, do not remove
		bivariate <- RR.bivariate(RR12, RR34, na.rm=na.rm, noCorrection=TRUE)$bivariate
		
		#Estimation of bivariate relations on construct level
		
		w <- getOption("warn")
		options(warn=-1)
		denom <- c(
			sqrt(lat1[1]*lat2[1]),
			sqrt(lat1[2]*lat2[2]),
			sqrt(lat1[1]*lat2[2]),
			sqrt(lat1[2]*lat2[1]),
			sqrt (lat1[3]*lat2[3]),
			sqrt (lat1[3]*lat2[3])
		)
		options(warn=w)
		
		bivariate$standardized <- clamp(bivariate$estimate / denom)

		# erase covariances if one variance component is < 0
		if (lat1[1] <= 0) bivariate[c(1,3),3:6] <- NaN
		if (lat1[2] <= 0) bivariate[c(2,4),3:6] <- NaN
		if (lat2[1] <= 0) bivariate[c(1,4),3:6] <- NaN
		if (lat2[2] <= 0) bivariate[c(2,3),3:6] <- NaN
		if (lat1[3] <= 0) bivariate[c(5,6),3:6] <- NaN
		if (lat2[3] <= 0) bivariate[c(5,6),3:6] <- NaN
		
		
		univariate <- list()
		univariate[[1]] <- lat1.full
		univariate[[2]] <- lat2.full
		
		grandres <- list(univariate = univariate, bivariate = bivariate)
		class(grandres) <- "RR"
		grandres$anal.type <- "Bivariate analysis of two constructs, each measured by two round robin variables"
		attr(grandres, "group.size") <- nrow(RRMatrix2)
		return(grandres)		
	}
	
	# if no condition above had a hit: error
	stop("Error: parameters are wrongly specified!")
	
}


 


# weighted variance, inspired by a function from Gavin Simpson on R-Help
var.wt <- function(x, w, na.rm = FALSE) {
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    sum.w <- sum(w)
    return((sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2)))
}




weighted.t.test <- function(x, w, mu, conf.level = 0.95, alternative="two.sided", na.rm=TRUE) {
	
	if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
        stop("'conf.level' must be a single number between 0 and 1")
	
	if (na.rm) { 
		w <- w[i <- !is.na(x)] 
		x <- x[i] 
	}
	
	# to achieve consistent behavior in loops, return NA-structure in case of complete missings
	if (sum(is.na(x)) == length(x)) return(list(estimate=NA, se=NA, conf.int=NA, statistic=NA, df=NA, p.value=NA))
	
	# if only one value is present: this is the best estimate, no significance test provided
	if (sum(!is.na(x)) == 1) {
		warning("Warning weighted.t.test: only one value provided; this value is returned without test of significance!")
		return(list(estimate=x[which(!is.na(x))], se=NA, conf.int=NA, statistic=NA, df=NA, p.value=NA))
	}
	
	x.w <- weighted.mean(x,w, na.rm=na.rm)
	var.w <- var.wt(x,w, na.rm=na.rm)
	df <- length(x)-1
	t.value <- sqrt(length(x))*((x.w-mu)/sqrt(var.w))
	se <- sqrt(var.w)/sqrt(length(x))
	
	if (alternative == "less") {
		pval <- pt(t.value, df)
		cint <- c(-Inf, x.w + se*qt(conf.level, df) )
    }
    else if (alternative == "greater") {
		pval <- pt(t.value, df, lower.tail = FALSE)
		cint <- c(x.w - se * qt(conf.level, df), Inf)
    }
    else {
		pval <- 2 * pt(-abs(t.value), df)
		alpha <- 1 - conf.level
        cint <- x.w + se*qt(1 - alpha/2, df)*c(-1,1)
    }
	
	names(t.value) <- "t"
	return(list(estimate=x.w, se=se, conf.int=cint, statistic=t.value, df=df, p.value=pval))
}



# helper-functions
posOrNA <- function(x) {
	return(ifelse(x>=0, x, NA))
}



# uni1, uni2: univariate Analysen der beiden Konstrukte (Daten werden zum Standardisieren der bivariaten Koeffizienten gebraucht)

getWTest <- function(RR0, res1, typ="univariate", uni1=NA, uni2=NA, unstable=NA) {
	
	if (is.null(RR0)) return();
	
	if (typ=="univariate") {
		if (length(RR0$univariate)==2) {varComp <- RR0$univariate[[1]]$varComp} else {varComp <- RR0$varComp}
		varComp$p.value <- NA
		varComp$estimate <- NA

		for (v in names(table(res1$type))) {
			w.t <- weighted.t.test(res1$estimate[res1$type == v], res1$group.size[res1$type == v]-1, mu=0)
	
			varComp[varComp$type==v,]$estimate <- w.t$estimate
			varComp[varComp$type==v,]$se <- w.t$se
			varComp[varComp$type==v,]$t.value <- w.t$statistic
			varComp[varComp$type==v,]$p.value <- w.t$p.value
		}
		varComp$p.value[1:4] <- varComp$p.value[1:4] / 2
		# Varianzen nur einseitig testen (Voreinstellung bei weighted.t.test ist zweiseitig)

		
		
		# unstable variance im latent bivariaten Fall wird von außen in die Funktion gegeben
		if (!is.na(unstable)) {
			varComp[4,2] <- unstable
		}


		#standardized coefficients need special treatment ...
		varComp[1,3] <- posOrNA(varComp[1,2])/ sum(posOrNA(varComp[1:4,2]), na.rm=TRUE)
		varComp[2,3] <- posOrNA(varComp[2,2])/ sum(posOrNA(varComp[1:4,2]), na.rm=TRUE)
		varComp[3,3] <- posOrNA(varComp[3,2])/ sum(posOrNA(varComp[1:4,2]), na.rm=TRUE)
		varComp[4,3] <- posOrNA(varComp[4,2])/ sum(posOrNA(varComp[1:4,2]), na.rm=TRUE)
		w <- getOption("warn")
		options(warn=-1)
			varComp[5,3] <- varComp[5,2]/ sqrt(varComp[1,2]*varComp[2,2])
			varComp[6,3] <- varComp[6,2]/ varComp[3,2]
		options(warn=w)
		
		varComp[,3] <- clamp(varComp[,3])
		
		# variance below zero: erase all other indices
		bz <- which(varComp[1:3, 2]<0)
		if (length(bz)>0) {
			varComp[bz, 3:6] <- NaN
			if (varComp[1,2]<0 | varComp[2,2]<0) varComp[5,3:6] <- NaN
		}
		
		# error variance: set se, t, p to NA (instead of NaN)
		varComp[4,4:6] <- NA
	
		return(varComp)
	}
	
	if (typ=="bivariate") {
		
		
		bivariate <- RR0$bivariate
		bivariate$p.value <- NA

		for (v in names(table(res1$type))) {
			w.t <- weighted.t.test(res1$estimate[res1$type == v], res1$group.size[res1$type == v]-1, mu=0)
	
			bivariate[bivariate$type==v,]$estimate <- w.t$estimate
			bivariate[bivariate$type==v,]$se <- w.t$se
			bivariate[bivariate$type==v,]$t.value <- w.t$statistic
			bivariate[bivariate$type==v,]$p.value <- w.t$p.value
		}

		#standardized coefficients need special treatment ...
		w <- getOption("warn")
		options(warn=-1)
			bivariate[1,3] <- bivariate[1,2]/ sqrt(uni1[1,2]*uni2[1,2])
			bivariate[2,3] <- bivariate[2,2]/ sqrt(uni1[2,2]*uni2[2,2])
			bivariate[3,3] <- bivariate[3,2]/ sqrt(uni1[1,2]*uni2[2,2])
			bivariate[4,3] <- bivariate[4,2]/ sqrt(uni1[2,2]*uni2[1,2])
			bivariate[5,3] <- bivariate[5,2]/ sqrt(uni1[3,2]*uni2[3,2])
			bivariate[6,3] <- bivariate[6,2]/ sqrt(uni1[3,2]*uni2[3,2])
		options(warn=w)
		
		
		# erase covariances if one variance component is < 0
		if (uni1[1,2] <= 0) bivariate[c(1,3),3:6] <- NaN
		if (uni2[1,2] <= 0) bivariate[c(1,4),3:6] <- NaN
		if (uni1[2,2] <= 0) bivariate[c(2,4),3:6] <- NaN
		if (uni2[2,2] <= 0) bivariate[c(2,3),3:6] <- NaN

		bivariate[,3] <- clamp(bivariate[,3])
	
		return(bivariate)
	}
}





RR.multi.uni <- function(formule, data, na.rm=FALSE, verbose=TRUE) {

	# this function needs data in long format ...
	
	# parse formula
	if (is.null(data)) stop("If a formula is specified, an explicit data object has to be provided!");
	
	# f1 = formula without grouping factor
	fstring <- paste(as.character(formule)[c(2,1,3)], collapse=" ")
	f0 <- strsplit(gsub(" ","",fstring, fixed=TRUE),"\\|", perl=TRUE)[[1]]
	f1 <- formula(f0[1])
	group.id <- f0[2]

	# Vorlage für die Varianzkomponente erstellen
	df <- data[data[,group.id]==data[1,group.id],]
	RR0 <- RR(f1, data=df, verbose=FALSE, na.rm=na.rm)

	mode <- ifelse(length(RR0$univariate)==2,"bi","uni")
		
	res <- data.frame()
	res.bi <- data.frame()
	effect <- data.frame()
	effectRel <- data.frame()
	eff.gm <- data.frame();
	g.uni <- list()
	saa <- sbb <- scc <- sccs <- n.m <- c()
	undc1 <- unp1 <- unt1 <- unr1 <- un.raw  <- c()
	
	#data.groups <- long2matrix(formule, data, skip3=TRUE)
	
	for (g in names(table(data[,group.id]))) {
		
		#print(g)
		
		RR0 <- RR(f1, data=data[data[,group.id] == g,], verbose=FALSE, na.rm=na.rm, g.id=group.id)
		g.id <- g

		if (is.null(RR0)) {next;} else
		{RR1 <- g.uni[[g]] <- RR0}
		
		g.uni[[g]]$group.id <- g.id
		RR0$effects$group.id <- g.id
		effect <- rbind(effect, RR0$effects)

		RR0$effectsRel$group.id <- g.id
		effectRel <- rbind(effectRel, RR0$effectsRel)

		if (RR0$latent==FALSE) {
			RR0$effects.gm$group.id <- g.id
			eff.gm <- rbind(eff.gm, RR0$effects.gm)
		} else {
			eff.gm <- list(relationship=NA)
		}
		
		
		if (RR0$latent==FALSE) {
			saa <- c(saa, RR0$varComp[1,2])
			sbb <- c(sbb, RR0$varComp[2,2])
			scc <- c(scc, RR0$varComp[3,2])
			sccs <- c(sccs, RR0$varComp[6,2])
		} else {
			undc1 <- c(undc1, RR0$unstabdycov1)
			unp1 <- c(unp1, RR0$unstabper1)
			unt1 <- c(unt1, RR0$unstabtar1)
			unr1 <- c(unr1, RR0$unstabrel1)
		}
		
		n.m <- c(n.m, attr(RR0, "group.size"))
		
		u1 <- RR0$varComp
		u1$variable <- 1

		u1$group.size <-  attr(RR0, "group.size")
		u1$group.id <- g.id
		
		res <- rbind(res, u1)
		
	}



	# im latenten Fall: die Error variance erst am Ende berechnen (d.h., alle error componenten ueber alle Gruppen mitteln, die unter NUll auf Null setzen, dann addieren)
	
	unstable.raw.m <- NA
	if (RR1$latent==TRUE) {
		unstable.raw.m <- max(weighted.mean(unp1, n.m), 0) + max(weighted.mean(unt1, n.m), 0) + max(weighted.mean(unr1, n.m), 0)
	}	

	if (length(effect) == 0) {
		effect <- data.frame(actor=NA, partner=NA, relationship=NA)
	}
	# get weighted variance components
	varComp <- getWTest(RR1, res, unstable=ifelse(is.null(unstable.raw.m), NULL, unstable.raw.m))


	# calculate reliability for actor and partner effects, and variance of group means
	group.var <- NA
	
	if (!is.null(n.m)) {

		n <- mean(n.m)
		
		if (RR1$latent==FALSE) {
			saa.m <- weighted.mean(saa, n.m-1)
			sbb.m <- weighted.mean(sbb, n.m-1)
			scc.m <- weighted.mean(scc, n.m-1)
			sccs.m <- weighted.mean(sccs, n.m-1)
	
			rel.a <- saa.m / (saa.m + scc.m*(n-1)/(n*(n-2)) + sccs.m/(n*(n-2)))
			if (saa.m < 0) rel.a <- NaN
			rel.p <- sbb.m / (sbb.m + scc.m*(n-1)/(n*(n-2)) + sccs.m/(n*(n-2)))
			if (sbb.m < 0) rel.p <- NaN
		} else {
			
			
			unp1.m <- weighted.mean(unp1, n.m-1, na.rm=TRUE)
			unt1.m <- weighted.mean(unt1, n.m-1, na.rm=TRUE)
			unr1.m <- weighted.mean(unr1, n.m-1, na.rm=TRUE)
			undc1.m <- weighted.mean(undc1, n.m-1, na.rm=TRUE)
			
			
			r <- 2 # r = number of replications - in our case, it's always 2
			rel.a <- varComp$estimate[1] / ((varComp$estimate[1] + (unp1.m/r)) + (varComp$estimate[3]+(unr1.m/r))*(n-1)/(n*(n-2)) + (varComp$estimate[6]+(undc1.m/r))/(n*(n-2)))
			if (varComp$estimate[1] < 0) rel.a <- NaN

			
			rel.p <- varComp$estimate[2] / ((varComp$estimate[2] + (unt1.m/r)) + (varComp$estimate[3]+(unr1.m/r))*(n-1)/(n*(n-2)) + (varComp$estimate[6]+(undc1.m/r))/(n*(n-2)))
			if (varComp$estimate[2] < 0) rel.p <- NaN

			rel.r <- varComp$estimate[3] / (varComp$estimate[3]+(unr1.m/r))
			if (varComp$estimate[3] < 0) rel.r <- NaN
			
			attr(effectRel$relationship, "reliability") <- clamp(rel.r)
		}

		attr(effect[,2], "reliability") <- clamp(rel.a)
		attr(effect[,3], "reliability") <- clamp(rel.p)
		
		# group mean & group variance		
		group.means <- tapply(data[,all.vars(f1)[1]], data[,group.id], mean, na.rm=TRUE)
		group.var <- var(group.means) - ((varComp$estimate[1] + varComp$estimate[2] + 2*varComp$estimate[5])/n + (varComp$estimate[3]+varComp$estimate[6])/(n*(n-1)))
		
		
	}	
	
	#------------------------------------
	#--  Variance Components: calculate weighted mean and weighted between groups t-test
	#------------------------------------
	
	anal.type <- paste(RR1$anal.type, " in multiple groups",sep="")
	
	if (!is.null(varComp)) {
	
		res2 <- list(effects = effect, effectsRel = effectRel, effects.gm = eff.gm, varComp = varComp, groups = g.uni, varComp.groups=res, group.var=group.var, anal.type=anal.type)
		class(res2) <- "RRmulti"
		return(res2)
	} else {return();}
	
}



# mode = c("scatter", "bar")
plot.RRmulti <- function(x, ..., measure=NA, geom="scatter", conf.level=0.95, connect=FALSE) {
	RRm <- x
	
	if (is.na(measure)) measure <- localOptions$style
	library(ggplot2)
	
	mode <- ifelse(length(RRm$univariate)==2,"bi","uni")
	
	if (mode=="uni") {
		df0 <- RRm$varComp.group
		grouplevel <- RRm$varComp
	} else {
		df0 <- rbind(RRm$univariate[[1]]$varComp.group, RRm$univariate[[2]]$varComp.group)
		grouplevel <- rbind(cbind(RRm$univariate[[1]]$varComp, variable=1), cbind(RRm$univariate[[2]]$varComp, variable=2))
	}
	
	
	if (measure=="behavior") {lab <- gsub(" ", "\n", unilabels_b, fixed=TRUE)}
	if (measure=="perception") {lab <- gsub(" ", "\n", unilabels_p, fixed=TRUE)}
	
	# set right order and factor labels
	df <- df0
	df$type <- factor(df$type, levels=unilabels_b, labels=lab)
	
	
	
	if (geom=="scatter") {
		#define deterministic jittering
		df$jit.x <- NA
		for (i in names(table(df$group.id))) df$jit.x[df$group.id==i] <- rnorm(1,0,0.1)
		
		grouplevel$type2 <- factor(grouplevel$type, levels=unilabels_b, labels=lab)
		grouplevel$tcrit <- abs(qt((1-conf.level)/2,length(RRm$groups)-1))

		p1 <- ggplot(df, aes(y=estimate))

		p1 <- p1 + geom_point(aes(x=(as.numeric(type)+jit.x), size=group.size), alpha=0.6, color="grey60") + scale_size("Group size", to=c(3,8))
	
		p1 <- p1 + geom_point(aes(x=(as.numeric(type)+jit.x)), alpha=0.8, color="black", size=0.7)
		
		p1 <- p1 + scale_x_discrete()
		
		p1 <- p1 + geom_pointrange(data=grouplevel, aes(x=type2, y=estimate, ymin=estimate-tcrit*se, ymax=estimate+tcrit*se), color="darkgreen", size=1.1)
		
		
	
		# lines connecting the points (may be very cluttered)
		if (connect==TRUE) {
			p1 <- p1 +geom_line(data=df[as.numeric(df$type) <= 3,], aes(x=(as.numeric(type)+jit.x), y=estimate, group=group.id), color="grey40", alpha=0.7)
		}

	
		p1 <- p1 + opts(axis.text.x = theme_text(angle = 0, vjust=1), title=paste("Multiple round robin groups:\nAbsolute (co-)variance estimates\nand",round(conf.level,2)*100,"%-CI (weighted for group size)")) + xlab("")
		
		if (mode=="bi") {
			p1 <- p1 + facet_wrap(~variable)
		}
		
		return(p1)
	}	
	
	
	
	if (geom=="bar") {

			
		# df3 <- df
		# x <- df3[df3$group.id==1,]
		# ddply(df3, .(group.id), function(x) {return(x$standardized[x$type=="actor\nvariance"])})
		# 
		# df3$group2 <- factor(
		# 	df3$group.id, levels=df3$group.id[df3$type=="actor\nvariance"][order(df3$standardized[df3$type=="actor\nvariance"], na.last=FALSE)],
		# 	ordered=TRUE)
		#df3$group2 <- factor(df3$group.id, levels=df3$group.id[order(df3$standardized[df3$type=="actor\nvariance"])])
		
		df3 <- na.omit(df)
		
		p2 <- ggplot(df3[as.numeric(df3$type)<=3,], aes(x=group.id, y=standardized, fill=as.character(gsub("\n", " ", type, fixed=TRUE)))) + geom_bar(aes(width=group.size))
		p2 <- p2 + scale_fill_discrete("Variance Component") + ylab("Standardized variances")
		
		if (mode=="bi") {p2 <- p2 + facet_wrap(~variable)}
		
		return(p2)
	}
	
}


plot.RRbi <- function(x, ...) {
	plot.RRuni(x, ...)
}

plot.RRuni <- function(x, ..., measure=NA, geom="bar") {
	
	RRu <- x
	if (is.na(measure)) measure <- localOptions$style
	
	library(ggplot2)
	
	if (measure=="behavior") {lab <- unilabels_b}
	if (measure=="perception") {lab <- unilabels_p}
	
	
	mode <- ifelse(length(RRu$univariate)==2,"bi","uni")
	
	if (mode=="uni") {
		df <- RRu$varComp
	} else {
		df <- rbind(cbind(RRu$univariate[[1]]$varComp, variable=1), cbind(RRu$univariate[[2]]$varComp, variable=2))
	}
	
	df$type2 <- lab
	df$standardized[is.na(df$standardized)] <- 0
	
	p1 <- ggplot(df[df$type2 %in% lab[1:3],], aes(x=factor(1), y=standardized, fill=as.character(type2))) + geom_bar(stat="identity") + scale_fill_discrete("Variance Component") + xlab("") + ylab("Standardized variance component")
	
	if (geom=="pie") p1 <- p1 + coord_polar(theta="y")
	
	if (mode=="bi") {p1 <- p1 + facet_wrap(~variable)}
	
	return(p1)
}


plot_missings <- function(formule, data, show.ids=TRUE) {

	library(ggplot2)
	
	# parse formula
	if (is.null(data)) stop("If a formula is specified, an explicit data object has to be provided!");
	
	f0 <- all.vars(formule)
	grs <- long2matrix(formule, data, reduce=FALSE)
	
	m1 <- melt(grs)
	m1$value2 <- factor(ifelse(is.na(m1$value), 1, 0))
	colnames(m1)[1:2] <- c(f0[2], f0[3])
	m1[,f0[2]] <- factor(m1[,f0[2]], ordered=TRUE)
	m1[,f0[3]] <- factor(m1[,f0[3]])
	
	p2 <- ggplot(m1, aes_string(x=f0[3], y=f0[2])) + geom_point(aes(color=value2)) + facet_wrap(~L1, scales="free")
	p2 <- p2 + scale_colour_identity("Missing?", breaks=c(0,1), labels=c("yes", "no"), legend=TRUE) 
	
	p2 <- p2 + opts(axis.text.x = theme_text(angle = 90, hjust=1, size=7), axis.text.y = theme_text(hjust=1, size=7), title="Missing values", aspect.ratio=1)
	
	if (show.ids==FALSE) {
		p2 <- p2 + opts(axis.text.x = theme_text(size=0), axis.text.y = theme_text(size=0))
	}
	return(p2)
}


RR.multi <- function(formule, data, na.rm=FALSE, verbose=TRUE) {

	# this function needs data in long format ...
	
	# parse formula
	if (is.null(data)) stop("If a formula is specified, an explicit data object has to be provided!");
	
	# f1 = formula without grouping factor
	fstring <- paste(as.character(formule)[c(2,1,3)], collapse=" ")
	f0 <- strsplit(gsub(" ","",fstring, fixed=TRUE),"\\|", perl=TRUE)[[1]]
	f1 <- formula(f0[1])
	group.id <- f0[2]
	
	f3 <- strsplit(strsplit(gsub(" ","",fstring, fixed=TRUE),"~", perl=TRUE)[[1]][1], "+", fixed=TRUE)[[1]]
	f4 <- strsplit(gsub(" ","",fstring, fixed=TRUE),"~", perl=TRUE)[[1]][2]

	# Vorlage für die Varianzkomponente erstellen
	df <- data[data[,group.id]==data[1,group.id],]
	RR0 <- RR(f1, data=df, verbose=FALSE, na.rm=na.rm)

	mode <- ifelse(length(RR0$univariate)==2,"bi","uni")



	if (mode=="uni") return(RR.multi.uni(formule, data, na.rm, verbose))

	# ... ansonsten bi-mode durchführen
	
	V1 <- RR.multi.uni(formula(paste(f3[1], "~", f4)), data, na.rm, verbose)
	V2 <- RR.multi.uni(formula(paste(f3[2], "~", f4)), data, na.rm, verbose)
	V2$varComp.groups$variable <- 2
		
	res <- data.frame()
	res.bi <- data.frame()
	bi.groups <- list()
	
	for (g in names(table(data[,group.id]))) {
		
			RR0 <- RR(f1, data=data[data[,group.id] == g,], verbose=FALSE, na.rm=na.rm)
			
			if (is.null(RR0)) {next;} else
			{RR1 <- bi.groups[[g]]  <- RR0}
			
			if (!is.null(RR1$bivariate)) {
				res.bi <- rbind(res.bi, data.frame(RR1$bivariate, group.size=attr(RR1, "group.size"), group=g))
			}
		
	}

	
	anal.type <- paste(RR1$anal.type, "in multiple groups")
	
	bivariate <- getWTest(RR1, res.bi, typ="bivariate", V1$varComp, V2$varComp)
	
	res <- list(univariate = list(V1, V2), bivariate = bivariate, anal.type=anal.type, groups=bi.groups)
	class(res) <- "RRmulti"

	return(res)


}





getEffects <- function(formule, data, varlist, by=NA, na.rm=TRUE) {

	# run a RR analysis for each variable and store results in a list
	res_list <- list()
	for (v in 1:length(varlist)) {
		print(paste("Calculate:",varlist[v]))
		f1 <- formula(paste(varlist[v], paste(as.character(formule), collapse="")))
		RR1 <- RR(f1, data=data, na.rm=na.rm, verbose=FALSE)
		res_list <- c(res_list, list(RR1$effects))
	}


	# now combine all effects in a single data frame; merge by id
	library(reshape)
	
	if (is.na(by)) {
		if (length(RR1$groups) > 1) {by <- c("id", "group.id")} 
		else {by <- "id"}
	}
	
	res <- merge_recurse(res_list, by=by)
}




print.RRmulti <- function(x, ...) {
	print.RR(x, ...)
}


print.RRuni <- function(x, ...) {
	print.RR(x, ...)
}

print.RRbi <- function(x, ...) {
	print.RR(x, ...)
}

# x muss hier direkt auf das univariate-Objekt verweisen
print.uni <- function(x, ..., measure="behavior", digits=3, r.names=NULL) {
	uni <- round(x$varComp[,2:ncol(x$varComp)], digits)
	
	if (!is.null(r.names)) {rownames(uni) <- r.names} else {
		if (measure == "behavior") rownames(uni) <- unilabels_b
		if (measure == "perception") rownames(uni) <- unilabels_p
		if (measure == "metaperception") {
			warning("Warning: the current RR-object only consists of a single variable. Labels for metaperception are only provided when two variables are specified.")
			rownames(uni) <- unilabels_b
		}
	}
	
	print(uni)
	
	# Actor effect reliability
	if (!is.null(x$effects[,2])) print(paste(role[[localOptions$style]][1], "effect reliability:",round(attr(x$effects[,2], "reliability"), 3)))
	
	# Partner effect reliability
	if (!is.null(x$effects[,3])) print(paste(role[[localOptions$style]][2], "effect reliability:",round(attr(x$effects[,3], "reliability"), 3)))
	
	# Relationship effect reliability
	if (!is.null(attr(x$effectsRel$relationship, "reliability"))) print(paste(role[[localOptions$style]][3], "effect reliability:",round(attr(x$effectsRel$relationship, "reliability"), 3)))
	
	# if (any(uni[1:2,2]<0.10) | any(is.na(uni[1:2,2]))) print(paste("Warning:",rownames(uni)[5],"should NOT be interpreted if standardized actor or partner variance is < 10%!"))
	# if (uni[3,2]<0.10 | is.na(uni[3,2])) print(paste("Warning:",rownames(uni)[6],"should NOT be interpreted if standardized relationship variance is < 10%!"))
}



# Here the default print method for RR-objects gets overwritten, so that 
# the information in the RR-class is displayed in a convenient way
print.RR <- function(x, ..., measure1=NA, measure2=NA, digits=3, measure=NULL) {
	
	if (is.na(measure1)) measure1 <- localOptions$style
	if (is.na(measure2)) measure2 <- measure1
	
	print("Round-Robin object ('RR'), calculated by Triple-R")
	print(x$anal.type)
	
	
	# print descriptivers for multi group
	if (length(x$groups) > 1) {
		groupsizes <- laply(x$groups, function(y) return(attr(y, "group.size")))
		av.groupsize <- round(mean(groupsizes), 2)
		
		 print(paste("Group descriptives: n = ",length(x$groups),"; average group size = ",av.groupsize, "; range: ", min(groupsizes), "-", max(groupsizes)))
	}
	
	if (!is.null(measure)) {measure1 <- measure}
	
	# bivariate case
	if (length(x$univariate) == 2) {
		
		uni <- lapply(x$univariate, function(x) return(x))
		bi <- round(x$bivariate[,2:ncol(x$bivariate)], digits)
		
		r.names1 <- r.names2 <- NULL
		if (measure1 == "behavior" & measure2 == "behavior") {
			rownames(bi) <- bilabels_bb
		} else
   		if (measure1 == "behavior" & measure2 == "perception") {
				rownames(bi) <- bilabels_bp
		} else
		if (measure1 == "perception" & measure2 == "behavior") {
				rownames(bi) <- bilabels_pb
		} else
		if (measure1 == "perception" & measure2 == "perception") {
				rownames(bi) <- bilabels_pp
		} else
		if (measure1 == "perception" & measure2 == "metaperception") {
			r.names1 <- unilabels_b_meta1
			r.names2 <- unilabels_b_meta2
			rownames(bi) <- bilabels_meta
		} else {
			stop("This combination of measurement labels does not fit.")
		}
		print(paste("Univariate analyses for:", attr(uni[[1]], "varname")))
		print.uni(uni[[1]], measure=measure1, r.names=r.names1)
		print(paste("Univariate analyses for:", attr(uni[[2]], "varname")))
		print.uni(uni[[2]], measure=measure2, r.names=r.names2)
		print("Bivariate analyses:")
		print(bi)
		
	} else
	
	# univariate case
	{
		print(paste("Univariate analyses for:", attr(x, "varname")))
		print.uni(x, measure=measure1)
	}
	
	
}
