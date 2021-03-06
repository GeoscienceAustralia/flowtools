% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/lasUtil.R
\name{getLasFilePointsInPolygon}
\alias{getLasFilePointsInPolygon}
\title{Extract las points inside a given polygon}
\usage{
getLasFilePointsInPolygon(myPol, lasFile, xRange = NULL, yRange = NULL,
  zRange = NULL)
}
\arguments{
\item{myPol}{- A SpatialPolygons/SpatialPolygonsDataFrame object or 2-column
matrix defining a polygon in which we want las points}

\item{lasFile}{- the las filename}

\item{xRange}{- length 2 vectors giving the min,max
range of x to extract from the las file (before selecting points in
the polygon). This can save time}

\item{yRange}{-- See xRange}

\item{zRange}{-- See xRange}
}
\value{
- Matrix of las points
}
\description{
Extract las points inside a given polygon
}
\examples{
lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
  # CRS of the data
  myCRS=CRS('+init=epsg:26914')
  # Get extent as a SpatialPolygons object
  mypol=getLasFileExtentPolygon(lasFile,myCRS)
  # Now make a polygon by trimming off the outer 100m
  mypol_shrunk=gBuffer(mypol,width=-100)
  # Extract lidar points in mypol_shrunk
  point_subset=getLasFilePointsInPolygon(mypol_shrunk, lasFile)
  plot(mypol,axes=TRUE)
  plot(mypol_shrunk,add=TRUE,border='blue')
  points(point_subset[,1],point_subset[,2],col=point_subset[,3]+3,pch='.')
  title('Subset of the point cloud')
  # Check points are in the polygon
  p1 = mypol_shrunk@polygons[[1]]@Polygons[[1]]@coords
  insiders=point.in.polygon( point_subset[,1],point_subset[,2],p1[,1],p1[,2])
  stopifnot(all(insiders==1))
}

