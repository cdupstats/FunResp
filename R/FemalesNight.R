#' An example list over 11 habitats available and used by roe deer
#'
#' A dataset containing the relative availabilities (RA) in the home ranges and the estimated relative use values (fit) 
#'  by female roe deer in the Bavarian Forest for midnight in June. Estimated use values originate from multicategory logit models.
#'
#' @format A list with 11 entries:
#' \describe{
#'   \item{RA}{values of relative availability in ascending order}
#' 	\item{fit}{estimated relative use values}
#' }
#' @source {Bavarian Forest Nationalpark}
#' @examples 
#' 
#' data(FemalesNight)
#'par(mfrow=c(4,3),mar=c(4,4,1,1),oma=c(2,2,2,2))
#'lapply(1:length(FemalesNight),function(X) plot(FemalesNight[[X]][,"RA"],FemalesNight[[X]][,"fit"],main=names(FemalesNight)[X],type="l",ylab="Proportion of use",xlab="Relative availability"))
#'
#'HT <- lapply(1:length(FemalesNight), function(X) optimHolling(FemalesNight[[X]][,"RA"], FemalesNight[[X]][,"fit"]))
#'names(HT) <- names(FemalesNight)
#'HTty <- do.call(rbind, HT)
#'
#'par(mfrow=c(4,3),mar=c(4,4,1,1),oma=c(2,2,2,2))
#'lapply(1:length(FemalesNight),function(ty) {
#'			x <- FemalesNight[[ty]][,"RA"]	
#'			plot(x,FemalesNight[[ty]][,"fit"],main=names(FemalesNight)[ty],type="l",ylab="Proportion of use",xlab="Relative availability")
#'			if(HTty[ty,"type"]==1){
#'				lines(x,Holling1(x,HTty[ty,"a"]),col="red", lty=3, lwd=3)
#'			}
#'			if(HTty[ty,"type"]==2){
#'				lines(x,Holling2(x,HTty[ty,"a"],HTty[ty,"b"]),col="red",lty=3,lwd=3)
#'			}
#'			if(HTty[ty,"type"]==3){
#'				lines(x,Holling3(x,HTty[ty,"a"],HTty[ty,"b"]),col="red",lty=3,lwd=3)
#'			}
#'			abline(a=0,b=1,col="darkgrey")
#'			mtext(paste("Hollingtype",c("I","II","III","IV")[HTty[ty,"type"]],"\n a=",round(HTty[ty,"a"],2),"\n b=",round(HTty[ty,"b"],2)),side=1,line=-2,adj=0.9, col = "red")
#'		})
#' 
"FemalesNight"
