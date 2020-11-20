# TODO: Add comment
# 
# Author: dupke
###############################################################################


#' Multicategory logit models for all habitats
#'
#' Calculates a given logistic regression models for all habitat types separately except from but the baseline category. Is used in function multicat1. 
#' It ensures that availability is set correctly for the focal habitat type. HRavail is calculated with HRti.  
#' @param Data data.frame supplies covariates named in the model 
#' @param HRavail a data.frame obtained with HRti
#' @param model model formula
#' @keywords cats
#' @export
#' @examples
#' # see data preparation in function HRti
#'  
#' HRavail <- HRti(Data,var="month",Landscapegrid,plot=TRUE)
#' #merge with data
#' Data$month_f <- as.character(floor(Data$month))
#' Data2 <- cbind(Data,HRavail[apply(Data[,c("burst","month_f")],1,paste,collapse="."),-c(1:2)])
#' Data2$avail <- unlist(sapply(1:nrow(Data2),function(i) HRavail[apply(Data2[i,c("burst","month_f")],1,paste,collapse="."),as.character(unlist(Data2[i,"habitat"]))]))
#' Data2$relavail <- Data2$avail/Data2$total
#' 
#' Data2$dum1 <- 1 #dummy variable
#' #Run models (for all habitats but the baseline category)
#' #set the model first
#' f1 <- formula(present~offset(log(rel.avail))
#' 			+s(hour,bs="cp")
#' 			+s(rel.avail,bs="ps")
#' 			+s(burst,by=dum1,bs="re"))
#' 
#'  nn <- table(Data2$habitat)
#' 
#' habitats <- names(nn)[which(nn>10)] #take only those with more than 10 recordings
#' Data2 <- subset(Data2,habitat %in% habitats)
#' Data2$habitat  <- factor(relevel(Data2$habitat, ref="old mixed")) #make sure that baseline habitat is at first position like relevel(Data$habitat, ref="old mixed")
#' MM <- multicat1(Data2,HRavail,f1,sp=c(1,2,1))



multicat1 <- function(Data,HRavail,model,...){
	
	habs <- levels(Data$habitat)[-1]
	ref <- levels(Data$habitat)[1] #baseline habitat
	#only data from focal habitat and baseline habitat
	DATA <- lapply(habs, function(ty) subset(Data,  habitat %in% c(ty,ref)))
	names(DATA) <- habs

	M <- lapply(DATA,modbinom, HRavail=HRavail, model=model,...)
	M
}

