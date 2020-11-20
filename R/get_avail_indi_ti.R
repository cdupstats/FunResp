	
#' Availability of habitats within time-indexed home ranges for one individual
#'
#' This function allows you to obtain the a list of the number of grid cells covered with each habitat type within a 100% mcp 
#' home ranges spanned by given x- and y- coordinates over well defined time intervals.  A SpatialGridDataFrame that provides landscape information is needed. 
#' It is advicable to generate the data.frame Dindi via adehabitatHR::ld(as.ltraj(data)). 
#' This function is used within \code{\link{HRti}}.
#' @param Dindi data.frame than contains x- and y- coordinates, the name of the individual as $burst and the times ti, over its levels the home ranges are calculated
#' @param Landscapegrid SpatialGridDataFrame where names of the habitat types are stored as names "habitat" in the data
#' @param discN takes only time intervals ti with more than discN recordings per time interval ti
#' @param plot (logical) wether you wish to see a plot of the home range within the landscape (time comsuming)
#' @keywords home ranges
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
#' get availability of habitat types in monthly home range
#' par(mfrow=c(3,4),mar=c(1,1,1,1),oma=c(1,1,1,1))
#' Df$ti <- floor(Df[,"month"])		
#' #get availability of habitats within time-indexed home ranges
#' TTihm <- table(Df[,c("burst","ti")])
#' 
#' #for one individual
#' get_avail_indi_ti(Dindi=subset(Df,burst==unique(Df$burst)[1]),Landscapegrid,discN=5,plot=TRUE)
#' #for all individuals
#' HRavail <- lapply(unique(Df$burst),function(ii) get_avail_indi_ti(Dindi=subset(Df,burst==ii),Landscapegrid,discN=10,plot=TRUE))


get_avail_indi_ti <- function(Dindi,Landscapegrid,discN=0,plot=FALSE){
		tt <- table(Dindi$ti)
		#take only entries with more than discN recordings per time interval
		momo <- as.numeric(names(which(tt > discN)))
			
		DF_m_indi <- lapply(momo,function(x) Dindi[which(Dindi$ti==x),c("x","y","burst","ti")])
		AV_m <- lapply(DF_m_indi,get_avail,Landscapegrid=Landscapegrid,plot=plot)
		names(AV_m) <- as.character(momo)
		return(AV_m)
	}
