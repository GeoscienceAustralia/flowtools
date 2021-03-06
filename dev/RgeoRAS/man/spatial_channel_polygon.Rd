% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/spatial_hec_util.R
\name{spatial_channel_polygon}
\alias{spatial_channel_polygon}
\title{Create a channel polygon from a hec-ras geometry file}
\usage{
spatial_channel_polygon(hec_chan_file, spatial_proj)
}
\arguments{
\item{hec_chan_file}{The name of a hecras geometry file (e.g. extension
.g01), or a character vector made from this with readLines}

\item{spatial_proj}{A CRS object defining the spatial projection}
}
\value{
A spatial polygons data frame with the channel outline, where each
       channel has a unique integer ID value
}
\description{
Function to read a hec-ras file, extract the channel network, and make a
SpatialPolygonsDataFrame which covers it
}
\examples{
require(rgdal)
  mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
  myChanPoly=spatial_channel_polygon(mychanfile, CRS('+init=epsg:3123'))
  plot(myChanPoly,axes=TRUE)
}

