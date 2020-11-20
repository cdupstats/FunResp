#' Return value where use equals availability
#'
#' 
#' @param a parameter describing Holling's curve (I, II or III)
#' @param b parameter describing Holling's curve (II or III)
#' @param type the type of Holling's equation (1, 2 or 3)
#' @return x_star the value, where use = availability, for type I it is 0, for type II one value, for type III a vector of two values
#' @export
#' @examples
#' 
#' x <- seq(0,1, length=100)
#' a <- 0.2
#' b <- 0.05
#' plot(x, Holling2(x,a,b), type ="l", ylim =c(0,a))
#' abline(h = a)
#' abline(v = b, lty = 2)
#' abline(h = a/2, lty = 2)
#' lines(x,x,col="darkgrey")
#' xstar <- get_xstar(a,b,2)
#' points(xstar, xstar, pch = 8, col = "red")
#'  
#' 
#' a <- 0.4
#' b <- 0.15
#' plot(x, Holling3(x,a,b), type ="l", ylim =c(0,a))
#' abline(h = a)
#' abline(v = b, lty = 2)
#' abline(h = a/2, lty = 2)
#' lines(x,x,col="darkgrey")
#' xstar <- get_xstar(a,b,3)
#' points(xstar[1], xstar[1], pch = 8, col = "red")
#' points(xstar[2], xstar[2], pch = 8, col = "red")
#' 

get_xstar <- function(a,b,type){
	root <- 0
	if(type == 2) root <- a-b
	if(type == 3) root <- c(a/2 - sqrt((a/2)^2 - b^2),a/2 + sqrt((a/2)^2 - b^2))
	return(root)
}