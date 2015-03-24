require(rgdal)
require(raster)
require(rgeos)

######################################################################################################
#
# Utility function for use in updating manning values
#
update_manning_from_manN_SPDF<-function(section, 
                                        section_xy_SP,
                                        manN_SPDF
                                        ){
    # Make vector which is zero for points outside of the conveying region, and 1 for points inside 
    #xy_in_manN_SPDF=1-is.na(over(section_xy_SP, manN_SPDF) )

    # Set manning n values for every  point on the cross-section
    man_values=unlist(over(section_xy_SP, manN_SPDF))
    #channel_regions=which(section[,1]>=bankl & section[,1]<=bankr)
    #man_values[channel_regions]=mann_chan
    if(all(is.na(man_values))) return(NULL)
    if(any(is.na(man_values))) stop('NA Manning -- perhaps some xsections are partially covered by the shapefiles?')

    # Compute indices at which we need to define a manning n value for hec-ras
    changepts=c(1,which(diff(man_values)!=0)+1)

    manning_info=cbind(section[changepts,1],man_values[changepts], rep(0,length(changepts)))

    # Format for hec-ras
    mann_out=pad_string( as.character(t(manning_info)), charlen=8, justify='right')
    mann_out = format_in_rows(mann_out, 9)

    mann_header=paste('#Mann= ', length(changepts),' ,-1 , 0 ',sep="")
    mann_out=c(mann_header,mann_out)
    return(mann_out)
}

######################################################################################################
# 
# Utility function for merging multiple shapefiles defining manning's n
#
merge_mann_SpPolys_with_preferences<-function(SpPolys_list, mann_list, preference_order){
    # Merge SpatialPolygons with different manning n values into a single SpatialPolygonsDataFrame
    # , with 'preferred' polygons occuring earlier in the object (so they are preferentially picked up by 'over')

    if(length(SpPolys_list)!=length(mann_list) | length(mann_list)!=length(preference_order)){
        stop("Number of SpPolys should equal number of manning n's and length of preference_order")
    }

    # Fix polygons to have unique ID's
    counter=0
    for(i in 1:length(SpPolys_list)){
        for(j in 1:length(SpPolys_list[[i]]@polygons)){
            SpPolys_list[[i]]@polygons[[j]]@ID=as.character(counter)
            counter=counter+1
        }
    }
  
    k= which.min(preference_order)
    merged_shp=SpPolys_list[[k]]
    merged_shp_sp=as(merged_shp, 'SpatialPolygons')
    merged_shp_sp_n=rep(mann_list[[k]], length(merged_shp_sp))

    ll = length(preference_order) 
    for(i in sort(preference_order)[2:ll]){
        k=which(preference_order==i)
        newshp=as(SpPolys_list[[k]], 'SpatialPolygons')
        merged_shp_sp_n=c(merged_shp_sp_n, rep(mann_list[[k]], length(newshp)))
        merged_shp_sp=rbind(merged_shp_sp, newshp)

    }

    outSPDF=SpatialPolygonsDataFrame(merged_shp_sp, data=data.frame(n=merged_shp_sp_n), match.ID=FALSE)
    return(outSPDF) 

}

###################################################################
#
# Utility function to get spatial coordinates from a cross-sectional profile, using its cutline
#
get_section_xy_coords<-function(section, section_cutline, 
                         section_alignment_tol=glob_vars$section_alignment_tol){
    # Get the xy coordinates of a cross-section in plan view. These are defined
    # by 1) The section cutline, and 2) The cross-channel distances
    # 
    # We check if the section is consistent with the section cutline
    # Because our hecras stores x-coords to 2 decimal places, we allow for some error
    
    cutline_length=c(0, cumsum( ( diff(section_cutline[,1])**2 + diff(section_cutline[,2])**2)**0.5))
    cutline_length_coord=cutline_length+min(section[,1]) # Cross-channel distance coordinate

    # Compute the difference between the cutline length and the length of the 'section' coordinates
    # Note that this can occur due to digitization, OR, for automatically generated profiles,
    # it is because we evenly space points on a multi-segment cutline. This may miss some corners.
    # Then, the cutline and profile will have slightly different lengths 
    length_error=abs(max(cutline_length_coord) - max(section[,1]))

    if(length_error > section_alignment_tol){
        print('length error')
        print(max(cutline_length_coord))
        print(max(section[,1]))
        print(length_error)
        print('## HERE IS THE CUTLINE##')
        print(section_cutline)
        print('### HERE IS THE PROFILE ##')
        print(section)
        #browser()
        stop('ERROR: Cutline length is significantly different to section length.\n
              This means there is a problem with the georeferencing, which will \n
              prevent this routine from working correctly')
    }
    
    # Make functions to get x & y coordinates, based on knowing the cross-channel distance
    section_cutline_x = approxfun(cutline_length_coord, section_cutline[,1], rule=2)
    section_cutline_y = approxfun(cutline_length_coord, section_cutline[,2], rule=2)

    x = section_cutline_x(section[,1])
    y = section_cutline_y(section[,1])

    return(cbind(x,y))
}
##########################################################################
#
#' Code to set manning's n values from shapefile(s)
#'
#' @param hec_geo Filename of hecras geometry, or a character vector produced from such a file
#' @param SpPolys_list Either a list of shapefile filenames, OR a list of SpatialPolygons* objects, OR a single SpatialPolygons* object
#' @param mann_list A list or vector of manning's n values inside polygons in SpPolys_list. Must have the same length as SpPolys_list, with e.g. mann_list[[1]] being the manning n value inside SpPolys[[1]]
#' @param preference_order Vector of unique integers giving the 'preference-rank' for each SpPoly, which is used to set manning's n in case of overlaps. The length(preference_order) = length(SpPolys_list), and e.g. preference_order[1] is the rank of SpPolys_list[[1]], preference_order[2] is the rank of SpPolys_list[[2], etc. Polygons with lower values in preference_order are preferred to those with higher values. 
#' @param reaches_to_modify_pattern Vector of one or more characters. Only reaches with names that pattern-match one or more elements of reaches_to_modify_pattern will be affected by the function. 
#' @param output_file File to write the geometry to. If NULL, return the modified geometry as a character vector
#' @param verbose Print many messages
#' @return The modified geometry as a character vector, unless output_file!=NULL, in which case the modified geometry is written to a output_file.
#' @details Pattern-matching using \code{reaches_to_modify_pattern} is done by grep. Defaults to '*', which matches everything. For example, say we have three reaches 'Reach1', 'NewRiver', 'Reach2'. Then reaches_to_modify_pattern=c('Reach', 'New') will modify everything, reaches_to_modify_pattern=c('2') will only modify 'Reach2', reaches_to_modify_pattern='X' will not modify anything, etc.
#' @export
#' @examples
#'     mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/Geo_with_xsect.g02')
#'     manning_0p03=paste0(system.file(package='RgeoRAS'), '/extdata/Manning/Lower_delta.shp')
#'     manning_0p05=paste0(system.file(package='RgeoRAS'), '/extdata/Manning/Mann_chan.shp')
#'     manning_0p1=paste0(system.file(package='RgeoRAS'), '/extdata/Manning/Mann1.shp')
#'     # Prefer to use 0p03, then 0p05, then 0p1
#'     new_geo=edit_manning_from_shapefile(mychanfile, 
#'                                         list(manning_0p05, manning_0p03, manning_0p1), 
#'                                         list(0.05, 0.03, 0.1), 
#'                                         preference_order=c(2, 1, 3))
#'  
#' 
edit_manning_from_shapefile<-function(hec_geo, SpPolys_list, mann_list, 
                                     preference_order=1:length(mann_list),
                                     reaches_to_modify_pattern='*',
                                     output_file=NULL, verbose=TRUE){
   
    #@ check that preference_order is unique 
    if(max(table(preference_order))!=1) stop('Preference order is non-unique')
    
    #@ Get the geometry data
    if(length(hec_geo)==1 && file.exists(hec_geo)){
        if(verbose) print('Reading geometry ...')
        hec_lines=readLines(hec_geo)
    }else{
        hec_lines=hec_geo
    }
  
    #@ Ensure SpPolys_list / mann_list are actually lists
    if(class(SpPolys_list)!='list'){
        SpPolys_list=list(SpPolys_list)
    }
    if(class(mann_list)!='list') mann_list=as.list(mann_list)
 
    if(length(SpPolys_list)!=length(mann_list)){
        print(paste('Length(SpPolys_list): ', length(SpPolys_list)))
        print(paste('Length(mann_list): ', length(mann_list)))
        stop("Must have the same number of SpPolys and manning's n values")
    }

    if(length(preference_order)!=length(mann_list)){
        stop('preference_order and mann_list should be the same length')
    }

    #@ If SpPolys_list is a list of shapefile names, read them in
    for(i in 1:length(SpPolys_list)){
        if(class(SpPolys_list[[i]])=='character'){
            if(verbose) print('Reading SpPolys into list')
            mylayer=strsplit(basename(SpPolys_list[[i]]), '.shp')[[1]][1]
            SpPolys_list[[i]]=readOGR(SpPolys_list[[i]], layer=mylayer)
        }

    }

    #@ merge SpPolys_list and mann_list to a single SpatialPolygonsDataFrame
    #@ polygons with higher preference will occur earlier in this object. 
    #@ This allows relatively 'clean' use with the 'over' function later
    manN_SPDF=merge_mann_SpPolys_with_preferences(SpPolys_list, mann_list, preference_order)

    #@ Get key information from hecras geometry
    if(verbose) print('EXTRACTING CUTLINE INDICES FROM HECRAS FILE')
    GIS_cutline_bounds=get_cutline_index_bounds(hec_lines)

    if(verbose) print('EXTRACTING XSECTION XY INDICES FROM HECRAS FILE')
    xsect_bounds=get_xsection_xy_index_bounds(hec_lines)
    xsect_1=xsect_bounds[,1]

    if(verbose) print('EXTRACTING MANNING N INDICES FROM HECRAS FILE')
    mann_startend=get_mann_index_bounds(hec_lines)
    mann_start=mann_startend[,1]
    mann_end=mann_startend[,2]


    river_reaches=grep('River Reach=', hec_lines) # Reach names
    section_names=grep('Type RM Length', hec_lines) # Station names
    bank_sta=grep('Bank Sta=', hec_lines) # Bank stations

    #@ Vector of length xsect_1, with the index of the corresponding river reach
    river_reaches_xsect=xsect_1*NA
    for(i in 1:length(xsect_1)){
        river_reaches_xsect[i] = river_reaches[sum(river_reaches<xsect_1[i])] 
    }

    #@ MAIN CODE
    if(verbose) print('MAIN CODE')

    new_hec_lines=hec_lines
    offset=0
    ll = length(hec_lines)
    # Main loop
    for(i in 1:length(xsect_bounds[,1])){
        #print(paste('Section ', i))
        
        # Compute section label
        mysect=max(section_names[section_names<xsect_bounds[i,1]])
        section_name=strsplit(hec_lines[mysect], ',')[[1]][2]

        ## Bypass reaches that do not match any of the 'reaches_to_modify_pattern' 
        keep=vector_grep(reaches_to_modify_pattern, hec_lines[river_reaches_xsect[i]])
        if(keep==0){
            next
        }
        if(verbose) cat('.')
        ### Bypass reaches that are not in the 'section_name_range'
        #section_number=strsplit(section_name, '[*]')[[1]][1]
        #test_expression=paste(section_number, sections_to_change)
        #test_outcome=eval(parse(text=test_expression))
        #if(!test_outcome){
        #    #print(c('skipping ' , section_number))
        #    next
        #}

        # Get section profile 
        section_text=hec_lines[xsect_bounds[i,1]:xsect_bounds[i,2]]
        section_profile=matrix(split_nchars_numeric(section_text,8),ncol=2,byrow=T)

        # Get section cutline
        section_cutline_text=hec_lines[GIS_cutline_bounds[i,1]:GIS_cutline_bounds[i,2]]
        section_cutline=matrix(split_nchars_numeric(section_cutline_text,16),ncol=2,byrow=T)

        #print(i)
        #browser()
      
        # Get x-y coordinates of points on section profile 
        section_xy_coordinates= get_section_xy_coords(section_profile, section_cutline)
        # Coerce to spatial points class 
        section_xy_SP =  SpatialPoints(section_xy_coordinates,proj4string=manN_SPDF@proj4string)
     
       
        ## Extract bank values from hec-ras text file
        #tmp = hec_lines[bank_sta[i]]
        #tmp = strsplit(tmp, "=")[[1]][2]
        #tmp = strsplit(tmp, ",")[[1]]
        #bankl=as.numeric(tmp[1])
        #bankr=as.numeric(tmp[2])
     
        # New manning's n values
        new_manning= update_manning_from_manN_SPDF(section_profile, section_xy_SP,manN_SPDF)
        if(is.null(new_manning)) next
        
        ## INSERT CONTENTS 
        new_hec_lines = c(new_hec_lines[1:(mann_start[i]-1+offset)], new_manning, new_hec_lines[(mann_end[i] +1+offset):ll]) 
        # Offset+= number of lines in new_manning - number of lines in old manning
        offset=offset+length(new_manning) - (mann_end[i] - mann_start[i]+1)
    }
    if(verbose) cat('\n')

    if(is.null(output_file)){
        return(new_hec_lines)
    }else{
        heclines_2_File(new_hec_lines, output_file)
        return()
    }

}

