	
#' Availability of habitats within a home range
#'
#' This function allows you to obtain the number of grid cells covered with each habitat type within a 100% mcp 
#' home range spanned by given x- and y- coordinates. A SpatialGridDataFrame that provides landscape information is needed. 
#' It is advicable to generate the data.frame PP via ld(as.ltraj(data)). 
#' This function is used within \code{\link{get_avail_indi_ti}}.
#' @param PP data.frame than contains x- and y- coordinates, the name of the individual as $burst and if a plot is wished the time as $ti
#' @param Landscapegrid SpatialGridDataFrame where names of the habitat types are stored as names "habitat" in the data
#' @param plot (logical) wether you wish to see a plot of the home range within the landscape (time comsuming)
#' @keywords home ranges
#' @export
#' @examples
#' ...

get_avail <- function(PP,Landscapegrid,plot=FALSE){
			#if(PP)
			M  <- adehabitatHR::mcp(SpatialPoints(cbind(PP$x,PP$y)),percent=100)
			bb<- bbox(M)
			cs <- Landscapegrid@grid@cellsize
			cc <- bb[,1] + (cs/2)    # cellcentre
			cd <- ceiling(diff(t(bb))/cs)  # cells.dim
			h_grid <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
			h_SG <- SpatialGrid(h_grid) #M@grid#
			#browser()
			LUgrid1 <- sp::over(SpatialPoints(h_SG),Landscapegrid)
			if(plot){
				image(SpatialGridDataFrame(h_SG,data.frame(as.numeric(LUgrid1[[1]]))),main=paste(PP$burst[1],PP$ti[1],sep=" "))	
				points(cbind(PP$x,PP$y))
			}
			#absolute availability
			tab <- table(LUgrid1$habitat)
			#relative availability
			#return(tab/sum(tab))
			return(tab)
	}

