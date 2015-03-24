require(sp)
require(rgdal)
require(raster)
require(rgeos)

#' Create a channel polygon from a hec-ras geometry file
#'
#' Function to read a hec-ras file, extract the channel network, and make a
#' SpatialPolygonsDataFrame which covers it

#' @param hec_chan_file The name of a hecras geometry file (e.g. extension
#'                      .g01), or a character vector made from this with readLines
#' @param spatial_proj A CRS object defining the spatial projection
#' @return A spatial polygons data frame with the channel outline, where each
#'        channel has a unique integer ID value
#' @export
#' @examples
#'   require(rgdal) 
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   myChanPoly=spatial_channel_polygon(mychanfile, CRS('+init=epsg:3123'))
#'   plot(myChanPoly,axes=TRUE) 
spatial_channel_polygon<-function(hec_chan_file, spatial_proj){
    require(sp)
    require(rgdal)

    #@ Read file if needed
    if(length(hec_chan_file)==1){
        fin=file(hec_chan_file, open='r')
        hec_lines=readLines(fin)
        close(fin)
    }else{
        hec_lines=hec_chan_file
    }

    #@ Identify channels
    chan_ind=grep('River Reach=', hec_lines)
    chan_polygons=list()

    #@ For each channel, extract the cross-sectional start/end points
    #@ lb = start (lower bound), ub = end (upper bound)
    for(i in 1:length(chan_ind)){
        offset=chan_ind[i]-1
        lb=chan_ind[i]
        if(i<length(chan_ind)){
            ub=chan_ind[i+1]
        }else{
            ub=length(hec_lines)
        }
        #@ Only grep the relevant cross-sections
        xsect_start=grep('XS GIS Cut Line=', hec_lines[lb:ub]) + offset
        #xsect_end=grep('Node Last Edited Time=', hec_lines[chan_ind[i]: chan_ind[i+1]]) + offset
        xsect_end=grep('#Sta/Elev=', hec_lines[lb:ub]) + offset - 1

        # Treat the case of channels without xsections
        if(length(xsect_start)==0){
            err_mess = paste('ERROR: The following reach appears to not have any cross-sections', 
                             '\n', hec_lines[chan_ind[i]], '\n 
                               This will cause hec_help.R to fail :( \n',
                               ' Try adding 2 artificial cross-sections to the reach, or deleting it')
            stop(err_mess)
        }
        
        # Logical check that start and ends of x-sections line up.
        for(j in 1:length(xsect_start)){
            if(xsect_start[j]>xsect_end[j]){
                stop('ERROR: xsect_start is greater than xsect_end.  Check that
                      all cutlines in your hec-ras geometry file are georeferenced')
            }
        }
   
        #@ NOW EXTRACT THE START AND END POINTS OF EACH CROSS-SECTION 

        #@ Predefine variables to store start and end points of
        #@ cross-sections
        coords_start=c()
        coords_end=c()
        for(j in 1:length(xsect_start)){  # Loop over all xsections
            cutline_text=hec_lines[(xsect_start[j]+1):(xsect_end[j]-1)]
            coords=c()
            for(k in 1:length(cutline_text)){ # Loop over all lines 
                coords1=split_nchars_numeric(cutline_text[k],16)
                coords=rbind(coords,matrix(coords1,ncol=2,byrow=TRUE))
            }
            #print(coords)
            coords_start=rbind(coords_start,coords[1,])
            coords_end=rbind(coords_end,coords[length(coords[,1]),])
        }

        #@ Combine coordinates and make a closed polygon
        coords_all=rbind(coords_start, coords_end[length(coords_end[,1]):1,], coords_start[1,])

        #@ Append to list of polygons, in a way that makes it easy to produce a
        #@ spatialpolygonsdataframe later

        chan_polygons[[i]] = (Polygons(list(Polygon(coords_all)), ID=as.character(i)))
        if(chan_polygons[[i]]@area ==0){
            print( 
                 paste(c( '##############################################################################\n',
                          'ERROR: zero area for channel polygon ', i,' , ', hec_lines[chan_ind[i]], '\n',
                          'This probably means that the reach has less than 2 cross-sections \n',
                          'You need at least 2 cross-sections on every reach \n',
                          '##############################################################################\n',
                          ' '))
                 )
            chan_polygons[[i]]=NULL
        }
        
    }
        # Catch errors in reach definition
        if(any( unlist ( lapply(chan_polygons, is.null) ) ) ){
            stop('Will not proceed until you fix the above reaches')
        }
        #@ Now coerce output to SpatialPolygonsDataFrame
        chan=SpatialPolygons(chan_polygons, proj4string=spatial_proj)

        chan2=SpatialPolygonsDataFrame(chan, data=data.frame(id=seq(1,length(chan_polygons))), match.ID=FALSE)


        return(chan2)
}

############################################################################################

#' Extract storage areas from a hec-ras file
#' 
#' Function to extract storage areas from an existing hec-ras file
#' into a SpatialPolygonsDataFrame
#' Useful to plot them up, to help with the creation of new storage areas.
#'
#' @param hec_file Name of a hecras geometry file (typically with a .gXX
#'               extension, where XX is an integer), or a character vector containing the
#'               contents of such a file
#' @param spatial_proj A CRS object describing the projection (or a character
#'        string, in which case CRS(spatial_proj) will be used)
#' @return A SpatialPolygonDataFrame, with the storage area geometry, and its name in hec-ras 
#' @export
#' @examples 
#'   require(rgdal)
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   myStoragePoly=spatial_storage_areas(mychanfile, CRS('+init=epsg:3123'))
#'   plot(myStoragePoly,axes=TRUE) 

spatial_storage_areas<-function(hec_file,spatial_proj){
    require(sp)
    require(rgdal)

    # Early interface assumed spatial_proj was character.
    # Changed now for generality
    if(is.character(spatial_proj)) spatial_proj=CRS(spatial_proj)

    #@ Read file if needed
    if(length(hec_file)==1){
        fin=file(hec_file, open='r')
        hec_lines=readLines(fin)
        close(fin)
    }else{
        hec_lines=hec_file
    }
   
    store_coords_start=grep('Storage Area Surface Line', hec_lines) +1
    store_coords_end=grep('Storage Area Type', hec_lines) -1

    #@ Treat case with no storage areas
    if(length(store_coords_start)==0) return(NULL) 

    #@ Loop over all storage areas, and add them to a SpatialPolygonsDataFrame
    poly_list=list()
    storage_names=c()
    for(i in 1:length(store_coords_start)){
        storage_name=hec_lines[store_coords_start[i]-2]
        storage_name=strsplit(storage_name, '=')[[1]][2]
        storage_name=strsplit(storage_name,',')[[1]][1]
        
        coord_text=hec_lines[store_coords_start[i]:store_coords_end[i]]
        coords_out=c()
        for(j in 1:length(coord_text)){
            #print(coord_text[j])
            coords_out=c(coords_out,split_nchars_numeric(coord_text[j],16))
        }
        coords_out=matrix(coords_out,ncol=3,byrow=T)
        coords_out=rbind(coords_out, coords_out[1,])

        poly_list[[i]]=Polygons(list(Polygon(coords_out[,1:2])), ID=storage_name)
        storage_names=c(storage_names, storage_name) 
    }
    storage_sp_poly=SpatialPolygons(poly_list, proj4string=spatial_proj)

    storage_sp_polydf=SpatialPolygonsDataFrame(storage_sp_poly, data=data.frame(name=storage_names), match.ID=FALSE)
    return(storage_sp_polydf)
}


###############################################################################################

#' Make a shapefile of channel boundary points from a hecras file
#'
#' Make a shapefile of channel boundary points from a hecras file. The boundary
#' points define the end-points of the x-sections
#' 
#' @param hec_chan_file File name of hecras geometry, or a vector containing the contents of a file
#' @param spatial_proj CRS object defining the spatial coordinate system, OR a
#'         character string with the proj4string info (in which case CRS(spatial_proj) is used )
#' @return SpatialPointsDataFrame with boundary points of xsections
#'        The attribute table includes "reach_name", "station_name", "bank",
#'         "downstream_distance", "bank_elev", "line_num" (a cross-section id)
#' @export
#' @examples
#'   require(rgdal)
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   my_boundary_points=spatial_channel_boundary_points(mychanfile, CRS("+init=epsg:3123"))
#'
spatial_channel_boundary_points<-function(hec_chan_file,spatial_proj){
    #@ Produce a shapefile containing the end points of the cross-sections 
    require(sp)
    require(rgdal)

    if(is.character(spatial_proj)) spatial_proj=CRS(spatial_proj)

    #@ Read file if needed
    if(length(hec_chan_file)==1){
        fin=file(hec_chan_file, open='r')
        hec_lines=readLines(fin)
        close(fin)
    }else{
        hec_lines=hec_chan_file
    }

    #@ Identify channels
    chan_ind=grep('River Reach=', hec_lines)

    chan_polygons=list()

    #@ For each channel, extract the cross-sectional start/end points
    output_coords=c()
    output_data=c()
    for(i in 1:length(chan_ind)){

        offset=chan_ind[i]-1
        lb=chan_ind[i]
        if(i<length(chan_ind)){
            ub=chan_ind[i+1]
        }else{
            ub=length(hec_lines)
        }

        #@ Only grep the relevant cross-sections
        xsect_start=grep('XS GIS Cut Line=', hec_lines[lb:ub]) + offset
        #xsect_end=grep('Node Last Edited Time=', hec_lines[chan_ind[i]: chan_ind[i+1]]) + offset
        xsect_end=grep('#Sta/Elev=', hec_lines[lb:ub]) + offset - 1
      
        if(length(xsect_start)==0){
            err_mess = paste('ERROR: The following reach appears to not have any cross-sections', 
                             '\n', hec_lines[chan_ind[i]], '\n 
                               This will cause hec_help.R to fail :( \n',
                               ' Try adding 2 artificial cross-sections to the reach, or deleting it')
            stop(err_mess)
        }
        # Useful to have the 'station' lines as well. Initially I tried to use
        # this as the definition of xsect_start, however, I decided that was
        # not the best. However, we need this line to get the station number
        station_start=grep('Type RM Length L Ch R =', hec_lines[lb:ub]) + offset
   
        #@ NOW EXTRACT THE START AND END POINTS OF EACH CROSS-SECTION 

        #@ Predefine variables to store start and end points of
        #@ cross-sections
        coords_start=c()
        coords_end=c()
        reach_name=c()
        station_name=c()
        left_bank_downstream_dist=c()
        right_bank_downstream_dist=c()
        left_bank_elev=c()
        right_bank_elev=c()
        line_num=c()
        #@ Loop over all xsections and extract required data
        for(j in 1:length(xsect_start)){  
            cutline_text=hec_lines[(xsect_start[j]+1):(xsect_end[j]-1)]
            #@ Get cutline coordinates
            coords=c()
            for(k in 1:length(cutline_text)){ # Loop over all lines 
                coords1=split_nchars_numeric(cutline_text[k],16)
                coords=rbind(coords,matrix(coords1,ncol=2,byrow=TRUE))
            }
            coords_start=rbind(coords_start,coords[1,])
            coords_end=rbind(coords_end,coords[length(coords[,1]),])
            # Get reach name
            reach_name=c(reach_name, strsplit(hec_lines[offset+1], "=")[[1]][2] )
            # Get station name
            station_index=max(station_start[station_start<=xsect_start[j]])
            station_name=c(station_name, strsplit(hec_lines[station_index], ",")[[1]][2] )
            
            # Get left and right bank downstream distances
            left_bank_downstream_dist=c(left_bank_downstream_dist, strsplit(hec_lines[station_index], ",")[[1]][3] )
            right_bank_downstream_dist=c(right_bank_downstream_dist, strsplit(hec_lines[station_index], ",")[[1]][5] )
            
            # Get elevation associated with left and right banks
            sei=xsect_end[j]+1 # Index of the '#Sta/Elev' line, just before the station-elevation data
            # FIXME: Here we are assuming that there are < 300 station elevation lines -- probably true, but be careful!
            mi = grep('#Mann=',hec_lines[sei:(sei+300)])[1]+sei-1 # Index of the #Mann= line after the station-elevation data 
            xsect_sta_elev_inds=(sei+1):(mi-1) # Indices of the station - elevation data
            xsect_station_elevation=split_nchars_numeric(hec_lines[xsect_sta_elev_inds], 8.) # Each number takes up 8 characters
            left_bank_elev = c(left_bank_elev, xsect_station_elevation[2])
            right_bank_elev = c(right_bank_elev, xsect_station_elevation[length(xsect_station_elevation)])
            line_num = c(line_num,xsect_start[j])
        }
       
        output_coords=rbind(output_coords, coords_start)
        output_data=rbind(output_data, 
                          cbind(reach_name, station_name, 
                                rep('L', length(reach_name)), 
                                left_bank_downstream_dist,
                                left_bank_elev,
                                line_num))
        output_coords=rbind(output_coords, coords_end)
        output_data=rbind(output_data, cbind(reach_name, station_name, 
                                             rep('R', length(reach_name)), 
                                             right_bank_downstream_dist, 
                                             right_bank_elev,
                                             line_num))
        
        #print(cbind(station_name, reach_name))
    
    }
        # Coerce to spatial points
        output_pts=SpatialPointsDataFrame(coords=output_coords[,1:2], 
                      data=data.frame(reach_name=output_data[,1], station_name=output_data[,2],
                                      bank=output_data[,3], downstream_distance=as.numeric(output_data[,4]),
                                      bank_elev=as.numeric(output_data[,5]), line_num=as.numeric(output_data[,6])),
                      match.ID=FALSE,
                      proj4string=spatial_proj)
        return(output_pts)
}

###############################################################################################

#' Make a shapefile of channel cross-section points from a hecras file
#'
#' Make a shapefile of points on channel cross-sections from a hecras file.
#' 
#' @param hec_chan_file File name of hecras geometry, or a vector containing the contents of a file
#' @param spatial_proj CRS object defining the spatial coordinate system, OR a
#'         character string with the proj4string info (in which case CRS(spatial_proj) is used )
#' @return SpatialPointsDataFrame with points on xsections
#'        The attribute table includes "elevation", "station_name", 
#'         "station_index", "reach_name"
#' @export
#' @examples
#'   require(rgdal)
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   my_xsect_points=spatial_crossSection_points(mychanfile, CRS("+init=epsg:3123"))
#'
spatial_crossSection_points<-function(hec_chan_file,spatial_proj){
    #@ Produce a shapefile containing the end points of the cross-sections 
    require(sp)
    require(rgdal)

    if(is.character(spatial_proj)) spatial_proj=CRS(spatial_proj)

    #@ Read file if needed
    if(length(hec_chan_file)==1){
        fin=file(hec_chan_file, open='r')
        hec_lines=readLines(fin)
        close(fin)
    }else{
        hec_lines=hec_chan_file
    }

    #@ Identify channels
    chan_ind=grep('River Reach=', hec_lines)

    chan_polygons=list()

    #@ For each channel, extract the cross-sectional points
    output_coords=c()
    output_data=c()
    for(i in 1:length(chan_ind)){

        offset=chan_ind[i]-1
        lb=chan_ind[i]
        if(i<length(chan_ind)){
            ub=chan_ind[i+1]
        }else{
            ub=length(hec_lines)
        }

        #@ Only grep the relevant cross-sections
        xsect_start=grep('XS GIS Cut Line=', hec_lines[lb:ub]) + offset
        #xsect_end=grep('Node Last Edited Time=', hec_lines[chan_ind[i]: chan_ind[i+1]]) + offset
        xsect_end=grep('#Sta/Elev=', hec_lines[lb:ub]) + offset - 1
      
        if(length(xsect_start)==0){
            err_mess = paste('ERROR: The following reach appears to not have any cross-sections', 
                             '\n', hec_lines[chan_ind[i]], '\n 
                               This will cause hec_help.R to fail :( \n',
                               ' Try adding 2 artificial cross-sections to the reach, or deleting it')
            stop(err_mess)
        }
        # Useful to have the 'station' lines as well. Initially I tried to use
        # this as the definition of xsect_start, however, I decided that was
        # not the best. However, we need this line to get the station number
        station_start=grep('Type RM Length L Ch R =', hec_lines[lb:ub]) + offset
        pt_coords=c() 
        pt_elev=c()
        pt_station_name=c()
        pt_station_index=c()
        pt_reach_name=c()
        #@ NOW EXTRACT THE POINTS ON EACH CROSS-SECTION 
        #@ Loop over all xsections and extract required data
        for(j in 1:length(xsect_start)){  
            cutline_text=hec_lines[(xsect_start[j]+1):(xsect_end[j]-1)]
            #@ Get cutline coordinates
            coords=c()
            for(k in 1:length(cutline_text)){ # Loop over all lines 
                coords1=split_nchars_numeric(cutline_text[k],16)
                coords=rbind(coords,matrix(coords1,ncol=2,byrow=TRUE))
            }
            #@ Get the cumulative distance along the cutline
            coords_len=c(0, cumsum((diff(coords[,1])**2+diff(coords[,2])**2)**0.5))
    
            #browser()        
            # Get reach name
            reach_name=strsplit(hec_lines[offset+1], "=")[[1]][2] 
            # Get station name
            station_index=max(station_start[station_start<=xsect_start[j]])
            station_name=strsplit(hec_lines[station_index], ",")[[1]][2]
            
            # Get elevation associated with left and right banks
            sei=xsect_end[j]+1 # Index of the '#Sta/Elev' line, just before the station-elevation data
            # FIXME: Here we are assuming that there are < 300 station elevation lines -- probably true, but be careful!
            mi = grep('#Mann=',hec_lines[sei:(sei+300)])[1]+sei-1 # Index of the #Mann= line after the station-elevation data 
            xsect_sta_elev_inds=(sei+1):(mi-1) # Indices of the station - elevation data
            xsect_station_elevation=split_nchars_numeric(hec_lines[xsect_sta_elev_inds], 8.) # Each number takes up 8 characters
            xsect_pt_dist_elev=matrix(xsect_station_elevation,ncol=2,byrow=T)
            # Make sure that the max distance along the cutline = max distance along coordinates
            along_cut_dist=max(coords_len) 
            along_xsect_dist=max(xsect_pt_dist_elev[,1])-min(xsect_pt_dist_elev[,1])
            coords_len=coords_len/along_cut_dist*along_xsect_dist
            # Get xy coordinates along the cutline
            minX=min(xsect_pt_dist_elev[,1])
            fx=approx(coords_len, coords[,1], xout=xsect_pt_dist_elev[,1]-minX)$y
            fy=approx(coords_len, coords[,2], xout=xsect_pt_dist_elev[,1]-minX)$y
            # Stack up output data
            pt_coords=rbind(pt_coords, cbind(fx,fy))
            pt_elev=c(pt_elev, xsect_pt_dist_elev[,2])
            np=length(fx) # number of points
            pt_station_name=c(pt_station_name, rep(station_name, np))
            pt_station_index=c(pt_station_index, rep(station_index, np))
            pt_reach_name=c(pt_reach_name, rep(reach_name, np))

        }
       
        output_coords=rbind(output_coords, pt_coords)
        output_data=rbind(output_data, 
                          data.frame(pt_elev, pt_station_name, pt_station_index,pt_reach_name, stringsAsFactors=FALSE))
        
    
    }
        # Coerce to spatial points
        output_pts=SpatialPointsDataFrame(coords=output_coords[,1:2], 
                      data=data.frame(elevation=output_data[,1], station_name=output_data[,2],
                                      station_index=output_data[,3], reach_name=output_data[,4], stringsAsFactors=FALSE),
                      match.ID=FALSE,
                      proj4string=spatial_proj)
        return(output_pts)
}
##########################################################################################

#' Make a shapefile of channel cross-sections in plan view (cutlines) from a hecras file
#'
#' Make a shapefile of channel cross-sections from a hecras file. 
#' 
#' @param hec_chan_file File name of hecras geometry, or the content of such a file in a character vector
#' @param spatial_proj CRS object defining the spatial coordinate system, OR a
#'         character string with the proj4string info (in which case CRS(spatial_proj) is used )
#' @return SpatialPointsDataFrame with boundary points of xsections
#'        contains variables "id_2","sec_info","reach", with info from the hecras file
#' @export
#' @examples
#'   require(rgdal)
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   my_cutlines=spatial_channel_cutlines(mychanfile, CRS("+init=epsg:3123"))
#'
spatial_channel_cutlines<-function(hec_chan_file, spatial_proj){
    #@ Make a spatialLines object containing the cross-sectional cutlines
    require(sp)
    require(rgdal)

    if(is.character(spatial_proj)) spatial_proj=CRS(spatial_proj)

    #@ Read file if needed
    if(length(hec_chan_file)==1){
        fin=file(hec_chan_file, open='r')
        hec_lines=readLines(fin)
        close(fin)
    }else{
        hec_lines=hec_chan_file
    }

    #@ Identify cutlines
    cutline_start=grep('XS GIS Cut Line', hec_lines)+1
    cutline_end=grep('#Sta', hec_lines)-2

    if(length(cutline_start)!=length(cutline_end)){

        stop('ERROR: Cannot identify xsection cutlines. Make sure all xsections are georeferenced')
    }

    #browser()
    lines_list=list()
    for(i in 1:length(cutline_start)){
        #print('XX')
        #print(hec_lines[cutline_start[i]:cutline_end[i]])
        cutline=split_nchars_numeric(hec_lines[cutline_start[i]:cutline_end[i]], 16)
        cutline_2=matrix(cutline,byrow=T,ncol=2)

        lines_list[[i]] = Lines(list(Line(cutline_2)), ID=as.character(i))
    }

    #@ Identify associated reaches
    reaches=grep('River Reach', hec_lines)
    xsect_reaches=cutline_start*NA
    for(i in 1:length(xsect_reaches)){
        xsect_reaches[i] = reaches[ which.max( cumsum(reaches< cutline_start[i]) ) ]
    }

    xsect_labels=grep('Type RM Length L Ch R = 1', hec_lines) 

    xsect_cutlines=SpatialLines(lines_list,proj4string=spatial_proj)
    output=SpatialLinesDataFrame(xsect_cutlines, data=data.frame(id_2=1:length(xsect_cutlines), 
                                 sec_info=as.character(hec_lines[xsect_labels]), 
                                 reach=as.character(hec_lines[xsect_reaches])), match.ID=FALSE)
    return(output)
}

###################################################################

#' Make a polygon of channel junctions
#'
#' Make polygon of the junctions. This is needed for RAS Mapper type
#' plotting, and can be useful at other times too.
#'
#' @param hec_chan_file A hecras geometry file, or the contents of such a file as a character vector
#' @param channel_cutlines The output of spatial_channel_cutlines
#' @return A SpatialPolygonsDataFrame with the junction geometries 
#'        Contains variables "junct_name",  "xsect_inds",  "xsect_up_dn"
#' @export
#' @examples
#'   require(rgdal)
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   my_cutlines=spatial_channel_cutlines(mychanfile, CRS("+init=epsg:3123"))
#'   my_junctions=spatial_junction_polygon(mychanfile, my_cutlines)

spatial_junction_polygon<-function(hec_chan_file, channel_cutlines){
    # Make polygon of the junctions. This is needed for RAS Mapper type
    # plotting
    require(sp)
    require(rgdal)
    require(rgeos)

    #@ Read file if needed
    if(length(hec_chan_file)==1){
        fin=file(hec_chan_file, open='r')
        hec_lines=readLines(fin)
        close(fin)
    }else{
        hec_lines=hec_chan_file
    }

    # Indices associated with the start of each junction
    junct_starts=grep('Junct Name=', hec_lines)
    if(length(junct_starts)==0) return(NULL)
    # Hack to find the indices of the end of each junction
    junct_tmp=grep('Junc L&A',hec_lines)
    junct_ends=junct_tmp[which(diff(junct_tmp)>1)] # The ones we want are followed by several non-matching lines
    junct_ends=c(junct_ends, junct_tmp[length(junct_tmp)] ) 

    if( length(junct_starts)!=length(junct_ends) || any(junct_starts > junct_ends) ){
        stop('ERROR: Junction start/end indicies are not matching')   
    }

    # Get reach names for xsections
    cutline_reachnames=substring(channel_cutlines@data$reach, 13, 13+32-1)

    # Loop over every junction
    junct_tmp_polys=list() # Useful work list for junction polygons
    xsect_ind_store=rep(NA,length(junct_starts)) # This will hold the index of one xsection at the junction
    xsect_orientation_store=rep(NA,length(junct_starts)) # This will hold 'Up' / 'Dn' info for one xsection on each junction
    for(i in 1:length(junct_starts) ){

        # Get text / names of reaches in this junction
        junct_text=hec_lines[ junct_starts[i]:junct_ends[i] ]
        joining_reaches=grep('River,Reach=', junct_text)
        joining_reachnames=substring(junct_text[joining_reaches], 16, 16+32-1)

        # Record Up / Dn information: Up=1, Dn=0
        reach_orientation=joining_reaches*0    
        reach_orientation[ grep('Up River,Reach', junct_text[joining_reaches]) ] = 1

        # Get 'coordinates' of cutlines at junction
        #browser()
        coords=list()
        xsect_ind=rep(0,length(joining_reaches))
        for(j in 1:length(joining_reaches)){
            tmp = which(cutline_reachnames==joining_reachnames[j])
            # Downstream reach
            if(reach_orientation[j]==0){
                xsect_ind[j]=tmp[1]
            }else{
            # Upstream reach
                xsect_ind[j]=tmp[length(tmp)]
            }
            
            coords[[j]] = coordinates(channel_cutlines@lines[[ xsect_ind[j] ]])[[1]]
            
        }
        # Loop over joining reaches, find 'up' reaches, connect with all 'down' reaches
        # Then combine into a single polygon for each junction
        counter=0
        junct_tmp_poly=list()
        for(j in 1:length(joining_reaches)){
            if(reach_orientation[j]==1){
                # Preset variables for inner loop
                for(k in 1:length(joining_reaches)){
                    if(reach_orientation[k]==0){
                        # Make a polygon from the 2 xsections. To do this, flip
                        # the order of the downstream reach xsection
                        #counter=counter+1
                        counter=counter+1
                        rev_dn_coords=coords[[k]][ length(coords[[k]][,1]):1, 1:2]
                        # Store as spatial_polygons
                        junct_tmp_poly[[counter]]=SpatialPolygons( 
                                                     list( Polygons( 
                                                           list (Polygon( 
                                                                 rbind(coords[[j]], 
                                                                 rev_dn_coords, 
                                                                 coords[[j]][1,1:2]))
                                                           ), ID=as.character(counter))),
                                                proj4string=CRS(proj4string(channel_cutlines)))       
                    }

                }

            }
        }
        # Merge it into a single spatial polygon -- buffering to avoid topology exceptions
        for(j in 1:length(junct_tmp_poly)){
            if(j==1){
                junct_tmp_polys[[i]]=gBuffer(junct_tmp_poly[[j]], width=0.)
            }else{
                #print(c(i,j))
                junct_tmp_polys[[i]]=gUnion(junct_tmp_polys[[i]], gBuffer(junct_tmp_poly[[j]], width=0.))
            }
        }
        # Store information on the first xsection in each junction, for later plotting
        xsect_ind_store[i] = xsect_ind[1]
        xsect_orientation_store=reach_orientation[1]

    } # End loop over all junctions

    # Now combine all the junctions into 1 spatial polygons object
    all_polygons=list() #apply(junct_tmp_polys, getpol<-function(x) x[[1]])
    for(i in 1:length(junct_tmp_polys)){
            all_polygons[[i]]=junct_tmp_polys[[i]]@polygons[[1]]
            all_polygons[[i]]@ID=as.character(i)
    }
    junct_polys=SpatialPolygons(all_polygons, proj4string=CRS(proj4string(channel_cutlines)))
    junct_polys_df=SpatialPolygonsDataFrame(junct_polys, data=data.frame(junct_name=hec_lines[junct_starts], xsect_inds=xsect_ind_store, xsect_up_dn=xsect_orientation_store), match.ID=FALSE)
    
    return(junct_polys_df) 
}

#########################################################################################

#' Make a polygon of channel centrelines
#'
#' Make polygon of the centrelines. This is nice for plotting among other things
#'
#' @param hec_chan_file A hecras geometry file, or the contents of that file in a character vector
#' @param spatial_proj A CRS object, or a character proj4string which can be converted to one
#' @return A SpatialLinesDataFrame with the junction geometries 
#'        contains the variables "id_2", "reach"
#' @export
#' @examples
#'   require(rgdal)
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   my_centlines=spatial_centrelines(mychanfile, CRS("+init=epsg:3123"))

spatial_centrelines<-function(hec_chan_file,spatial_proj){
    #@ Extract channel centrelines from hecras file

    require(sp)
    require(rgdal)
    if(is.character(spatial_proj)) spatial_proj=CRS(spatial_proj)

    #@ Read file if needed
    if(length(hec_chan_file)==1){
        fin=file(hec_chan_file, open='r')
        hec_lines=readLines(fin)
        close(fin)
    }else{
        hec_lines=hec_chan_file
    }

    #@ Identify reaches
    reaches=grep('River Reach', hec_lines)
    reach_coordinate_end=grep('Rch Text', hec_lines)

    if(length(reaches)!=length(reach_coordinate_end)){
        stop("ERROR in compute_new_downstream_distances: The start and end points of the reach coordinates are not matching up")
    }

    #@ Now extract the centreline coordiantes
    centrelines=list()
    for(i in 1:length(reaches)){
        centre_txt=hec_lines[(reaches[i]+2):(reach_coordinate_end[i]-1)]
        centre_coords=matrix(split_nchars_numeric(centre_txt, 16), ncol=2,byrow=T)    

        centrelines[[i]] = Lines(list(Line(centre_coords)), ID=hec_lines[reaches[i]])
    }
    
    reach_lines=SpatialLines(centrelines,proj4string=(spatial_proj))
    reach_lines=SpatialLinesDataFrame(reach_lines, data=data.frame(id_2=1:length(reach_lines), reach=as.character(hec_lines[reaches])), match.ID=FALSE)
    return(reach_lines) 
}

#' Make shapefiles of key hecras geometries
#'
#' Convenience function to make lots of shapefiles from hecras geometry
#'
#' @param hec_chan_file filename of hecras geometry, or a character vector containing the contents of such a file
#' @param spatial_proj either a CRS object containing the projection, or a character string which will be coerced to that with CRS( )
#' @param outdir directory to write the shapefiles to. By default this will be based on the name of hec_chan_file
#' @param overwrite TRUE/FALSE Overwrite existing shapefiles in outdir?
#' @return None, but creates many shapefiles as a side-effect
#' @export
#' @examples
#' \dontrun{
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   myheclines=readLines(mychanfile)
#'   spatial_hecgeo2shp(myheclines,CRS("init=epsg:3123"))
#' }
spatial_hecgeo2shp<-function(hec_chan_file,spatial_proj, outdir=NULL,overwrite=FALSE){
    # Make outdir
    
    #@ Read file if needed
    if(length(hec_chan_file)==1){
        fin=file(hec_chan_file, open='r')
        hec_lines=readLines(fin)
        close(fin)
    }else{
        hec_lines=hec_chan_file
        hec_chan_file='hecras-outputs'
    }
    
    if(is.null(outdir)) outdir=gsub('\\.', '-', paste0('spatial_', basename(hec_chan_file)))
    # Make spatial_proj a CRS object
    if(is.character(spatial_proj)) spatial_proj=CRS(spatial_proj)

    dir.create(outdir,showWarnings=FALSE)

    # Create/write shapefiles
    hec_centre=spatial_centrelines(hec_lines,spatial_proj)
    writeOGR(hec_centre,dsn=outdir,layer='chan_centrelines', 
        driver='ESRI Shapefile',overwrite_layer=overwrite, 
        check_exists=TRUE)

    hec_cutlines=spatial_channel_cutlines(hec_lines,spatial_proj)
    writeOGR(hec_cutlines,dsn=outdir,layer='chan_cutlines',
        driver='ESRI Shapefile',overwrite_layer=overwrite, 
        check_exists=TRUE)

    hec_polygon=spatial_channel_polygon(hec_lines,spatial_proj)
    writeOGR(hec_polygon,dsn=outdir,layer='chan_polygon', 
        driver='ESRI Shapefile', overwrite_layer=overwrite, 
        check_exists=TRUE)

    hec_pts=spatial_channel_boundary_points(hec_lines,spatial_proj)
    writeOGR(hec_polygon,dsn=outdir,layer='chan_boundary_pts', 
        driver='ESRI Shapefile',overwrite_layer=overwrite, 
        check_exists=TRUE)

    hec_storage=spatial_storage_areas(hec_lines, spatial_proj)
    writeOGR(hec_storage,dsn=outdir,layer='storage', 
        driver='ESRI Shapefile',overwrite_layer=overwrite, 
        check_exists=TRUE)

    hec_junc=spatial_junction_polygon(hec_lines,hec_cutlines)
    writeOGR(hec_junc,dsn=outdir,layer='junction', 
        driver='ESRI Shapefile',overwrite_layer=overwrite, 
        check_exists=TRUE)

    return()
}
