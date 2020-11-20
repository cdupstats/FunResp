#' Multicategory logit models
#'
#' Calculates a given logistic regression models for all habitat types separately except from the baseline category. Is used in function \code{\link{multicat1}}. 
#' It ensures that availability is set correctly for the focal habitat type. HRavail is calculated with HRti.  
#' @param D contains covariates only for the focal habitat type and the baseline category
#' @param HRavail a data.frame obtained with HRti
#' @param model model formula
#' @keywords cats
#' @export
#' @examples
#' # see multicat1


modbinom <- function(D, HRavail, model,...){
	#browser()
	ty <- levels(factor(D$habitat))[2]
	ref <- levels(factor(D$habitat))[1]
	D$present <- factor( ifelse(D$habitat==ref,0,1)) #make binomial data
	D$avail <- HRavail[apply(D[,c("burst","month_f")], 1, paste, collapse="."),ty]
	D <- subset(D, avail > 0)
	D$rel.avail <- D$avail/D$total
	D$log.rel.avail <- log(D$rel.avail)
	
	mgcv::gam(model ,data=D,
          family=binomial(link="logit"), method="REML", gamma=1.4,...)
	
}

