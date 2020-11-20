#' Function for Holling's type II
#'
#' 
#' @param x value 
#' @param a parameter for Holling type I (asymptote)
#' @param b parameter for Holling type I 
#' @return 
#' @keywords concave
#' @export
#' @examples
#' 
#' x <- seq(0,1, length=100)
#' a <- 0.8
#' b <- 0.3
#' plot(x, Holling2(x,a,b), type ="l", ylim =c(0,a))
#' abline(h = a)
#' abline(v = b, lty = 2)
#' abline(h = a/2, lty = 2)
#' lines(x,x,col="darkgrey")
#' 
#' par(mfrow =c(5,5),oma=c(4,4,0,0))
#' 
#' for(a in c(0.1,0.25,0.5,0.8, 1)){
#'	for(b in c(0.05,0.25,0.5,0.8, 1)){
#'		plot(x,Holling2(x,a,b),type="l",las=1,bty="l",xlab="",ylab="",mgp=c(1.2,0,0),cex.lab=4,lwd=4,ylim=c(0,1),yaxt="n",xaxt="n")#,main=paste("a=",a," , b=",b),cex.main=2)
#'		
#'		axis(1,c(0,0.5,1),c(0,0.5,1))
#'		axis(2,c(0,0.5,1),c(0,0.5,1))
#'		text(0.1,.95,paste("a=",a),col="red",cex=2)
#'		text(0.35,.95,paste( "b=",b),col="blue",cex=2)
#'		abline(h=a,col="red")
#'		abline(h=a/2,)
#'		abline(v=b,col="blue")
#'		#abline(v=a-b)
#'		axis(1,b,"b",col.lab="blue",cex.axis=2)
#'		#axis(1,a-b,"a-b",col.lab="blue",cex.axis=2)
#'		axis(2,a,"a",col.lab="red",las=1,cex.axis=2)
#'		axis(2,a/2,"a/2",col.lab="red",las=1,cex.axis=2)
#'	}}
#' title(ylab="Use",xlab="Availability",outer=TRUE,cex.lab=3,mgp=c(1.2,0,0))
#' 
 

Holling2 <- function(x,a,b){
	a*x/(b+x)
}