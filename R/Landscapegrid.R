#' SpatialGridDataFrame of a landscape in the Bavarain Forest Nationalpark
#'
#' A dataset containing the coordinates (x,y), id (burst) and time of recording (date).
#'
#' @format A SpatialGridDataFrame 271 x 338 cells à 10m x 10m grid cells. Variables measured for each grid cell are:
#' \describe{
#'   \item{Text_1}{original name of habitat type}
#' 	\item{habitats}{numeric coding of habitat categories}
#'   \item{habitat}{name of habitat types after aggregating Text_1 }
#'  }
#' @source {Bavarian Forest Nationalpark}
"Landscapegrid"