# find the correct effect based on its attribute
findEff <- function(df, type) {
	for (i in 1:ncol(df)) {
		if (!is.null(attr(df[,i], "type"))) {
			if (attr(df[,i], "type") == type) {return(i)}	
		} 
	}
	return(NA)
}

# x is an univariate RR-object
selfCor <- function(x, digits=3, measure=NA) {
		
	## print (partial) correlations with self ratings:
	if (attr(x, "self") == TRUE) {
		
		if (is.na(measure)) {
			measure <- localOptions$style
		} else {
			measure <- match.arg(measure, c("behavior", "perception", "metaperception"))
		}

		if ((var(x$effects[,findEff(x$effects, "actor")], na.rm=TRUE) == 0) | (var(x$effects[,findEff(x$effects, "partner")], na.rm=TRUE) == 0) | (var(x$effects[,findEff(x$effects, "self")], na.rm=TRUE) == 0)) {
			print("Warning selfCor- One or both variables have zero variance; skipping partial correlations with self ratings!")
			return(NULL);
		}	
		
		if (length(x$groups) <= 1) {	
			cat("\n\nCorrelations with self ratings:\n")
			c.a <- cor.test(x$effects[,findEff(x$effects, "actor")], x$effects[,findEff(x$effects, "self")], use="p")
			c.p <- cor.test(x$effects[,findEff(x$effects, "partner")], x$effects[,findEff(x$effects, "self")], use="p")
			res0 <- data.frame(r=c(c.a$estimate, c.p$estimate), t=c(c.a$statistic, c.p$statistic), df=c(c.a$parameter, c.p$parameter), p=c(c.a$p.value, c.p$p.value))
			res <- data.frame(r=f2(c(c.a$estimate, c.p$estimate), digits), t=f2(c(c.a$statistic, c.p$statistic)), df=c(c.a$parameter, c.p$parameter), p=f2(c(c.a$p.value, c.p$p.value), digits))
		} else {
			cat("\n\nPartial correlations with self ratings (controlled for group membership):\n")
			c.a <- parCor(x$effects[,findEff(x$effects, "actor")], x$effects[,findEff(x$effects, "self")], x$effects[,2])
			c.p <- parCor(x$effects[,findEff(x$effects, "partner")], x$effects[,findEff(x$effects, "self")], x$effects[,2])
			res0 <- data.frame(r=c(c.a$par.cor, c.p$par.cor), t=c(c.a$t.value, c.p$t.value), df=c(c.a$df, c.p$df), p=c(c.a$p, c.p$p))
			res <- data.frame(r=f2(c(c.a$par.cor, c.p$par.cor), digits), t=f2(c(c.a$t.value, c.p$t.value)), df=c(c.a$df, c.p$df), p=f2(c(c.a$p, c.p$p), digits))
		}
				
		rownames(res) <- paste("self rating with",role[[measure]][1:2],"effect")

		if (measure=="perception") {
			rownames(res)[1] <- rownames(res0)[1] <-paste(rownames(res)[1], "(assumed similarity)")
			rownames(res)[2] <- rownames(res0)[2] <-paste(rownames(res)[2], "(self-other agreement)")
		}
		
		print(res)
		cat("\n\n")
		
		return(invisible(res0))
	}

}
