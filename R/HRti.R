# TODO: Add comment
# 
# Author: dupke
###############################################################################

	
#' Availability of habitats within time-indexed home ranges
#'
#' This function allows you to obtain the a list of the number of grid cells covered with each habitat type within a 100% mcp 
#' home ranges spanned by given x- and y- coordinates over well defined time intervals for a group of individuals.
#' A SpatialGridDataFrame that provides landscape information is needed. 
#' It is advicable to generate the data.frame Df via adehabitatHR::ld(as.ltraj(data)). 
#' @param Df data.frame than contains x- and y- coordinates, the name of the individual as $burst and the times ti, over which levels the home ranges are calculated
#' @param varti column name of Df that encodes the time intervals. It can be continous, but is set to floor to ensure categorical time intervals
#' @param Landscapegrid SpatialGridDataFrame where names of the habitat types are stored as names "habitat" in the data
#' @param discN takes only time intervals ti with more than discN recordings per time interval ti
#' @param plot (logical) wether you wish to see a plot of the home range within the landscape (time comsuming)
#' @keywords home ranges
#' @return data.frame, each row refers to one individual and one time interval,
#'  columns are 
#' "burst",
#' "ti"(time interval),
#' "total" (number of grid cells that span the home range),
#' and for each habitat type one column that contains the number of grid cells overlayed by the habitat#' 
#' @export
#' @examples
#' data(Data)
#' data(Landscapegrid) #Landscapegrid needs column in datamatrix called "habitat"
#' 
#' Data$month <- as.numeric(format(Data$date,"%m"))
#' Data$hour <- as.numeric(format(Data$date,"%H"))
#' table(Data[,c("month","burst")])
#' get landscape information
#' Data$habitat  <- over(SpatialPoints(cbind(Data$x,Data$y)),Landscapegrid)$habitat
#' Data$habitat  <- relevel(Data$habitat, ref="old mixed")
#' 
#' #get availability of habitat types in monthly home range
#' par(mfrow=c(3,4),mar=c(1,1,1,1),oma=c(1,1,1,1))
#' Df$ti <- floor(Df[,"month"])		
#' #get availability of habitats within time-indexed home ranges
#' TTihm <- table(Df[,c("burst","ti")])
#' 
#' get availability of habitat types in monthly home range
#' par(mfrow=c(3,4),mar=c(1,1,1,1),oma=c(1,1,1,1))
#' HRavail <- HRti(Data,var="month",Landscapegrid,plot=TRUE)


HRti <- function(Df,varti,Landscapegrid,discN=0, plot=FALSE){
	
	Df$ti <- floor(Df[,varti])
		
#get background data
	TTihm <- table(Df[,c("burst","ti")])
	HRavail <- lapply(unique(Df$burst),function(ii) get_avail_indi_ti(Dindi=subset(Df,burst==ii),Landscapegrid,discN=discN,plot=plot))
	names(HRavail) <- unique(Df$burst)
	# sort data for each burst and level of ti
	monthused_indi <- lapply(HRavail,function(x) names(x))
	names(monthused_indi) <- names(HRavail)
	HRto <- data.frame(do.call(rbind,lapply(1:length(monthused_indi),function(x) cbind(burst=rep(names(monthused_indi)[x],length(monthused_indi[[x]])),
																							ti=monthused_indi[[x]]))))
	total <- unlist(lapply(HRavail,function(X) lapply(X,function(Y) sum(Y))))
	HRto$total <- total[apply(HRto,1,paste,collapse=".")]
	
	habs <- do.call(rbind,lapply(HRavail,function(X) do.call(rbind,X)))
	rownames(habs) <- 1:nrow(habs)
	if( all(apply(habs,1,sum)==HRto$total)) HRto <- cbind(HRto,habs)
		else stop("Internal error when combining total and availability: Contact maintainer of the function.")
	
	rownames(HRto) <- unname(apply(HRto[,c("burst","ti")],1,paste,collapse="."))
	HRto
}	
