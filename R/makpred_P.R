#' Prediction of selection probability
#'
#' Makes prediction of selection probability given the models calculated in \code{\link{multicat1}}. (The code needs to be adapted to the model formula)
#' *Caution*: For the prediction data is needed for all variables, but only one variable can be calculated over a range of values, otherwise you get a warning.
#' @param hab habitat type for which the selection probability is calculated
#' @param hoset  value(s) for variable "hour"
#' @param seqRA  value(s) for variable "rel.avail" (must be in the range of the data used for calculating the model) 
#' @keywords Prediction
#' @export
#' @examples
#' #' Run models (for all habitats but the baseline category)
#' set the model first
#' f1 <- formula(present~offset(log(rel.avail))
#' 			+s(hour,bs="cp")
#' 			+s(rel.avail,bs="ps")
#' 			+s(burst,by=dum1,bs="re"))
#' 
#' nn <- table(Data2$habitat)
#' 
#' habitats <- names(nn)[which(nn>10)] #take only those with more than 10 recordings
#' Data2 <- subset(Data2,habitat %in% habitats)
#' Data2$habitat  <- factor(relevel(Data2$habitat, ref="old mixed")) #make sure that baseline habitat is at first position like relevel(Data$habitat, ref="old mixed")
#' MM <- multicat1(Data2,HRavail,f1,sp=c(1,2,1))
#' names(MM) <- habitats[-1]
#' par(mfrow=c(3,6),mar=c(1,1,1,1),oma=c(1,1,1,1))
#' lapply(MM,plot)
#' 
#' #make predictions for functional repsonse
#' # set data for predictions
#' #sequence of relative availability for each habitat for which the prediction should be made (make sure that it does not exceed the range of the observed data)
#' seqRA <- sapply(habitats[-1],function(ty) try(seq(min(exp(MM[[ty]]$model$"offset(log(rel.avail))")),min(quantile(exp(MM[[ty]]$model$"offset(log(rel.avail))"),.95),0.9), length=100)))
#' seqRA <- cbind(seqRA,"old mixed"=seq(0.001,0.5,length=nrow(seqRA)))
#' #mean availability for each habitat
#' UP <- unique(Data2[,c("burst","habitat","relavail")])
#' ma3 <- aggregate(UP$relavail,by=list(UP$habitat),mean)
#' meanavail <- ma3$x
#' names(meanavail)  <- ma3$Group.1
#' meanavail <- meanavail/sum(meanavail)
#' # time
#' hour <- 0
#' #start predictions
#' Preds <- lapply(habitats[-1],makpred_P,hoset=hour,seqRA=seqRA,meanavail=meanavail,Mod=MM)
#' 
#' #plot predictions
#' lapply(Preds,function(X) plot(X[,"RA"],X[,"fit"],type="l"))
#' #make predictions over time
#' #start predictions
#' hour <- 0:23
#' RA <- matrix(rep(meanavail,each=length(hour)),nrow=length(hour),dimnames=list(c(),c(names(meanavail))))
#' Preds <- lapply(habitats[-1],makpred_P,hoset=hour,seqRA=RA,meanavail=meanavail,Mod=MM)
#' names(Preds) <- habitats[-1]
#' par(mfrow=c(2,4))
#' lapply(1:length(Preds),function(X) plot(Preds[[X]][,"hour"],Preds[[X]][,"fit"],type="l",ylim=c(0,.4),main=names(Preds)[X]))
#' 
#' # prediction for baseline category
#' predhour <- do.call(rbind,lapply(Preds,function(X) X[,"fit"]))
#' pred_base <- 1-apply(predhour,2,sum)
#' 
#' plot(hour,pred_base,type="l",ylim=c(0,.4),main=habitats[1])

makpred_P <- function(hab,hoset,seqRA,meanavail,Mod,ref=TRUE){
			#browser()			
			data <- expand.grid(hour=hoset,rel.avail=unique(seqRA[,hab]))#seq(0.001,ma[ty],length=5))#rel.avail=1/16)#
			data$burst <- Mod[[hab]]$model$burst[1]
			data$log.rel.avail <- log(data$rel.avail)
			data$dum1 <-  0 #no random effects
			
			types <- names(MM)
			
			Pred <- predict(Mod[[hab]],data,type="response")#data$rel.avail*exp(predict(MM[[ty]],data,type="lpmatrix") %*% coef(MM[[ty]]))
			foo <- function(rai){
					dat0 <- data[ rep(rai,length(types)-1),]
					rownames(dat0) <- types[-which(types==hab)]
					maa <- meanavail
					maa[hab] <- seqRA[rai,hab]
					K <- 1-maa[hab]
					kn <- which(names(maa)==hab)
					maa[-kn] <- (K/sum(maa[-kn]))*maa[-kn]
					dat0$rel.avail <- sapply(rownames(dat0),function(xx) maa[xx])
					dat0$log.rel.avail <- log(dat0$rel.avail)
					dat0
				}
				dattable <- do.call(rbind,lapply(1:nrow(seqRA),foo))
				datlisttype <- lapply( types[-which(types==hab)],function(ty) dattable[grep(ty,rownames(dattable),fixed=TRUE),])
				names(datlisttype) <- types[-which(types==hab)]
				#make predictions for all other habitats
				ORs <- sapply(names(datlisttype) ,function(ty2) predict(Mod[[ty2]],newdata=datlisttype[[ty2]],type="response"))#datlisttype[[ty2]]$rel.avail*exp(predict(MM[[ty2]],datlisttype[[ty2]],type="lpmatrix") %*% coef(MM[[ty2]]))) #datlisttype[[ty]][,"rel.avail"]*
					#OR0s <- sapply(names(datlisttype) ,function(ty2) predict(M0[[ty2]],newdata=datlisttype[[ty2]],type="response"))#datlisttype[[ty]][,"rel.avail"]*
				
				
			#get probabilities
			P <- sapply(1:length(Pred),function(x) Pred[x]/(1+Pred[x]+sum(ORs[x,]))) #sum(ORs[x,])+
			KK <- cbind(hour=data$hour,relavail=data$rel.avail,fit=P)
			return(KK)
}


