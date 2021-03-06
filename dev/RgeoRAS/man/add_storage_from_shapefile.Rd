% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/add_storage_from_shapefile.R
\name{add_storage_from_shapefile}
\alias{add_storage_from_shapefile}
\title{Add storage areas to the hecras geometry with a shapefile}
\usage{
add_storage_from_shapefile(hec_geo, new_storage, dem, dem_vertical_offset = 0,
  limit_weir_elevation_by_channel_bank_elevation = FALSE,
  lower_limit_on_lateral_weir_elevations = -Inf,
  upper_bound_storage_offset = 100, output_file = NULL, verbose = TRUE)
}
\arguments{
\item{hec_geo}{Filename of the hecras geometry, or a character vector produced from such a file with readLines}

\item{new_storage}{Filename of a polygon shapefile containing the new storage areas, or a SpatialPolygonsDataFrame with the new storage areas}

\item{dem}{Filename of a DEM to use to get the elevations in the storage areas, or a RasterLayer with the DEM}

\item{dem_vertical_offset}{= Number added to all values extracted from the DEM (to deal with datum offsets}

\item{limit_weir_elevation_by_channel_bank_elevation}{TRUE/FALSE Should the lateral weir elevation be forced to be > the channel bank elevation}

\item{lower_limit_on_lateral_weir_elevations}{numeric value which all lateral weir elevations are clipped to be greater than or equal to}

\item{upper_bound_storage_offset}{Offset to upper bound on stage-volume curves: see \code{upper_bound_stage_offset} in \code{compute_storage_stage_vol_relation}}

\item{output_file}{If not NULL, write the new hec-lines to a file with this name}

\item{verbose}{TRUE/FALSE -- verbosely print progress messages}
}
\value{
If \code{output_file=NULL}, then return a character vector with the new hecras geometry. Otherwise, write this character vector to a file, and return nothing.
}
\description{
Add storage areas to the hecras geometry with a shapefile
}
\details{
Storage areas are connected to each other if they touch when slightly buffered. See code for details.

     Storage areas are connected to the channel if they contain 2 or more bank points. If they touch the channel polygon without containing enough bank points, a message is printed.

     The hydraulic parameters for the storage area connections are randomly chosen, expect to have to edit these.
}
\examples{
mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/Geo_with_xsect.g02')
    mydem=paste0(system.file(package='RgeoRAS'), '/extdata/SRTM_clipped_PRS92Z3_comp.tif')
    myStoragePoly=paste0(system.file(package='RgeoRAS'), '/extdata/Storage/Store1.shp')
    new_ras_file=add_storage_from_shapefile(mychanfile, myStoragePoly, mydem)
}

