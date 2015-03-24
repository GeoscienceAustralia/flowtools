require(rgdal)
require(raster)
require(rgeos)

#' Add cross-sections to a hecras reach with a shapefile and a DEM
#'
#' The reach must already have a centreline in the hecras geometry,
#' and the reaches which will be modified should not contain any cross-sections. Not all important
#' inputs can be specified (junctions / manning / ect) so some further editing will be required.
#' 
#' @param hec_geo Either a filename (pointing to a hecras geometry file), or a character vector containing the hecras geometry. The latter could come from 'readLines(hecras_geometry_file)'
#' @param xsect_lines Either a line shapefile name (containing the plan-view geometries of the cross-sections we want to add to the reach), or a SpatialLines* object containing those geometries
#' @param dem Either a raster filename corresponding to the DEM, or a rasterLayer
#' @param dem_vertical_offset A number to add to the DEM values during processing
#' @param reaches_to_modify_pattern Character vector. Reaches will only be modified if they grep at least one pattern in reaches_to_modify_pattern 
#' @param outfile Filename to save the final geometry to
#' @param verbose TRUE/FALSE print progress messages
#' @param overwrite TRUE/FALSE -- overwrite outfile if it already exists?? Be careful
#' @param station_name_offset -- constant to add to the station_names (which will be real numbers). Useful because when merging channels, hecras requires the more usptream stations to have numbers greater than the more downstream stations.
#' @param default_manning Manning's n to use on the cross-section. Only a single value is supported, but see edit_manning_from_shapefile
#' @param ... Any other arguments to 'compute_crossSection_elev_with_dem'
#' @return If outfile is NULL, then return a character vector containing the updated hecras geometry. Otherwise, write the geometry to a file named 'outfile'.
#' @details WARNING: The cross-sectional profiles are computed with 'compute_crossSection_elev_with_dem'. Only a single downstream distance is set, and manning's n values are not specified. Use this to quicky get data into hec-ras, but assume you still need to check/edit the inputs.
#' @export
#' @examples
#'   partial_hecras_geofile=paste0(system.file(package='RgeoRAS'), '/extdata/centreline_only/make_xsect.g01')
#'   xsect_lines=paste0(system.file(package='RgeoRAS'), '/extdata/centreline_only/xsects/xsects.shp')
#'   lidar_DEM_file=paste0(system.file(package='RgeoRAS'), '/extdata/10m_dem.tif')
#'   lidar_DEM=raster(lidar_DEM_file)
#'   # Add cross-sections -- note use of optional argument to 'compute_crossSection_elev_with_dem' to control spacing between cross-sectional points
#'   new_ras_file=add_crossSections_to_reach(partial_hecras_geofile, xsect_lines, 
#'                    lidar_DEM, reaches_to_modify_pattern='*', desired_spacing=3.0)
#'   # print(new_ras_file)
#'   # cat(new_ras_file,'output.g01', sep="\r\n") # Output with carriage return good for windows
add_crossSections_to_reach<-function(hec_geo, xsect_lines,
                                     dem, dem_vertical_offset=0,
                                     reaches_to_modify_pattern=NULL,
                                     outfile=NULL, verbose=FALSE, overwrite=FALSE,
                                     station_name_offset=0., default_manning=0.03,
                                     ...){

    # Quick check of input
    if(is.null(reaches_to_modify_pattern)) stop('Must specify reaches_to_modify_pattern')
    # Check that outfile is OK
    if(!is.null(outfile)){
        if(file.exists(outfile) & !overwrite){
            stop('outfile already exists -- will not overwrite it unless overwrite=TRUE')
        }
    }

    # Check if hec_geo is a filename, or the hecras geometry. If the former,
    # read it in
    if(length(hec_geo)==1){
        if(file.exists(hec_geo)){
            hec_geo=readLines(hec_geo)
        }else{
            print(hec_geo)
            stop('hec_geo does not appear to be a hecras geometry character vector, or the filename of a hecras geometry file')
        }
    }

    # Read in xsect_lines if needed
    if(class(xsect_lines)=='character'){
        # xsect_lines is a shapefile -- read it in
        if(extension(xsect_lines)!='.shp') stop('xsect_lines must either be a shapefile (with .shp extension), or a SpatialLines / SpatialLinesDataFrame containing the cross-section cutlines')

        xsect_lines_dsn=dirname(xsect_lines)
        xsect_lines_layer=gsub(".shp", "", basename(xsect_lines))

        xsect_lines=readOGR(xsect_lines_dsn,xsect_lines_layer)                
    }

    # If dem is a filename, read it in
    if(class(dem)=='character'){
        dem=raster(dem)
    }

    # hecras geometry for editing
    hec_geo_edit=hec_geo

    # Find lines near the beginning/end of the reaches
    reach_start=grep('River Reach', hec_geo)
    reach_end=grep('Reverse River Text', hec_geo)

    if(length(reach_start)==0){
        stop('No River Reaches found in hec-geo -- is it really a hecras geometry file containing at least one reach??')
    }


    # Loop over all reaches
    output_text_reach=list()
    offset=0
    for(i in 1:length(reach_start)){
        #print(i)
        rs=reach_start[i]
        re=reach_end[i]
       
        # Check if reach name matches reaches_to_modify_pattern
        if(vector_grep(reaches_to_modify_pattern,hec_geo[rs])==0){
            next
        }

        # Get centreline xy coordinates
        r_xy=hec_geo[(rs+2):(re-2)]
        r_xy=split_nchars_numeric(r_xy,16)
        r_xy=matrix(r_xy,ncol=2,byrow=T)
        # Coerce to spatial lines
        r_xy_sp=SpatialLines(list(Lines(list(Line(r_xy)), ID=as.character(i))),
                             proj4string=xsect_lines@proj4string) 

        # Skip reaches with no intersections with the new cross-sections
        if(gIntersects(xsect_lines, r_xy_sp)==FALSE) next


        # Get the spatialLines which intersect
        tmp=gCrosses(xsect_lines, r_xy_sp, byid=T)
        xsect_lines_sub=SpatialLines(xsect_lines@lines[tmp], proj4string=xsect_lines@proj4string)
        # Get intersections of xsect_lines with the reach
        xsect_i=gIntersection(r_xy_sp, xsect_lines_sub, byid=T)

        if(verbose){
            print(' Working on : ')
            print(hec_geo[rs])
            print(' ')
        }

        # Check for existing cross-section
        if(length(grep('Type RM Length', hec_geo[rs:re]))>0){
            stop(paste0( hec_geo[rs] , ' contains cross-sections already. This routine requires that no cross-sections exist'))
        }

        # Compute upstream distances of the intersection
        usdist=rep(NA, length(xsect_i))
        connecting_segment=rep(NA, length(xsect_i))
        for(j in 1:length(xsect_i)){
            usdist_and_segment=usdistfun(xsect_i@coords[j,], r_xy)
            usdist[j] = usdist_and_segment[1]
            connecting_segment[j] = usdist_and_segment[2]
             
        }
        xsect_order=sort(usdist, decreasing=TRUE, index.return=TRUE) 
    
        downstream_distance=c(-diff(usdist[xsect_order$ix]), 0.) 


        # Loop over xsections from upstream to downstream, and calculate hecras
        # information.
        #output_text_reach[[i]]=c() # Define output text for this reach
        for(k in 1:length(xsect_i)){
            j = xsect_order$ix[k] # Index of 'k th' downstream xsection
            

            station_name=as.character(usdist[j]+station_name_offset)
            station_name=pad_string(station_name,8," ",justify='left')

            ##
            ## Need to correct cutline orientation: Should be left bank first,
            ## facing downstream
            ##
            cutline=coordinates(xsect_lines_sub@lines[[j]])[[1]] # Raw cutline
            # Get local tangent to stream, and normalise it
            kk=connecting_segment[j] # Segment on r_xy that cutline intersects with
            stream_tangent=r_xy[kk+1,1:2]-r_xy[kk,1:2]
            stream_tangent=stream_tangent/(sum(stream_tangent**2))**0.5 # Should point downstream

            cutline_tangent=(cutline[length(cutline[,1]),1:2]-cutline[1,1:2]) # Should be oriented 'left to right'
            cutline_tangent=cutline_tangent/(sum(cutline_tangent**2))**0.5

            # Test if tangent goes from left to right, facing downstream
            stream_tangent_rotated_right=c(stream_tangent[2], -stream_tangent[1])
            # If dot product is +, then cutline is oriented 'left to right'. If
            # not, flip the coordinate order
            if(sum( cutline_tangent*stream_tangent_rotated_right) < 0){
                ll = length(cutline[,1]):1
                cutline=cutline[ll,1:2]
            }

            cutline_len=sum(sqrt(diff(cutline[,1])**2 + diff(cutline[,2])**2))
            tmp_section=matrix(c(seq(0,cutline_len,len=10), rep(-1e+9,10)),ncol=2)
            # Get the cross-sectional profile
            section_profile=compute_crossSection_elev_with_dem(tmp_section, cutline, dem,
                                dem_vertical_offset, ...)

            ## Make cross-section text for hec-ras
            section_text= make_crossSection_text(station_name, cutline,section_profile,  
                                                 downstream_distance[k], mann=default_manning)
            # If k==1, create output text, otherwise append to it.
            if(k==1){
                output_text_reach[[i]] = section_text
            }else{
                output_text_reach[[i]] = c(output_text_reach[[i]], section_text)
            }                      
        }

        # Append to output
        ll = length(hec_geo_edit)
        hec_geo_edit=c(hec_geo_edit[1:(re+offset+1)], output_text_reach[[i]], hec_geo_edit[(re+offset+2):ll])
        offset=offset+length(output_text_reach[[i]])

    }

    # Return character output, or write to file
    if(is.null(outfile)){
        return(hec_geo_edit)
    }else{
        heclines_2_File(hec_geo_edit,outfile)
        return()
    }
    
}



