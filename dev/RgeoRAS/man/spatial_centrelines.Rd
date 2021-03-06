% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/spatial_hec_util.R
\name{spatial_centrelines}
\alias{spatial_centrelines}
\title{Make a polygon of channel centrelines}
\usage{
spatial_centrelines(hec_chan_file, spatial_proj)
}
\arguments{
\item{hec_chan_file}{A hecras geometry file, or the contents of that file in a character vector}

\item{spatial_proj}{A CRS object, or a character proj4string which can be converted to one}
}
\value{
A SpatialLinesDataFrame with the junction geometries
       contains the variables "id_2", "reach"
}
\description{
Make polygon of the centrelines. This is nice for plotting among other things
}
\examples{
require(rgdal)
  mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
  my_centlines=spatial_centrelines(mychanfile, CRS("+init=epsg:3123"))
}

