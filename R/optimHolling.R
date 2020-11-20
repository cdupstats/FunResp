#' Return optimal Holling's type 
#'
#' 
#' @param x vector of availability values in ascending order
#' @param y vector of use for the values of x
#' @return data.frame containing
#' @return  type the optimal Holling type (I, II or III)
#' @return a the associated parameter a 
#' @return b parameter b for Holling type II and III
#' @return x_star the value, where use = availability 
#' @return value optimization value 
#' @export
#' @examples
#' 
#'data(FemalesNight)
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

#' 

optimHolling <- function(x,y){
	
	min.RSSH1 <- function(data,par){
		sum((Holling1(data[,"x"],par[1])-data[,"y"])^2)
	}
	min.RSSH2 <- function(data,par){
		sum((Holling2(data[,"x"],par[1],par[2])-data[,"y"])^2)
	}
	
	min.RSSH3 <- function(data,par){
		sum((Holling3(data[,"x"],par[1],par[2])-data[,"y"])^2)
	}
	
	
	opt <- matrix(NA,ncol=3, nrow=3)
	OPT <- vector("list",3)
	OPT[[1]] <- optim(par=c(1),min.RSSH1, data=cbind(x,y), lower=0, upper=100, method="Brent")
	OPT[[2]] <- optim(par=c(1,0.2),min.RSSH2, data=cbind(x,y), lower=0, upper=1, method="L-BFGS-B")
	OPT[[3]] <- optim(par=c(1,0.2),min.RSSH3, data=cbind(x,y), lower=0, upper=1, method="L-BFGS-B")
		
	type <- which.min(c(OPT[[1]]$value,OPT[[2]]$value,OPT[[3]]$value))
	a <- OPT[[type]]$par[1]
	b <- OPT[[type]]$par[2]
	#get y=x
	#x_star <- get_xstar(a,b,type)
	
	data.frame(type=type,a = round(a,5),b = round(b,5), value = round(OPT[[type]]$value,5))
}