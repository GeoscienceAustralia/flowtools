#############################################################################

#' Make text representing a storage area in hec-ras format
#' 
#' Function to make a block of text representing a storage area,
#' as appears in the hec-ras format .g01
#' Probably, this will only be of internal use
#'
#' @param storage_area -- A SpatialPolygons/SpatialPolygonsDataFrame representing a single storage area
#' @param elev_vol  -- A stage-volume relation (typically output from 'compute_storage_stage_vol_relation')
#' @param name -- A name for the storage area that hec-ras will use
#' @return A lot of text
#' @export
#' @examples
#'     require(raster)
#'     require(rgdal)
#'     require(rgeos)
#'     mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'     mydem=paste0(system.file(package='RgeoRAS'), '/extdata/10m_dem.tif')
#'     lidar_DEM=raster(mydem)
#'     myStoragePoly=spatial_storage_areas(mychanfile, CRS('+init=epsg:3123'))
#'     myStageVolRel=compute_storage_stage_vol_relation(myStoragePoly[1,], lidar_DEM)
#'     #sat=make_storage_area_text(myStoragePoly[1,], myStageVolRel, 'Blah')
#'     #print(sat)
#'     
make_storage_area_text<-function(storage_area, elev_vol,name){
    #@ Function to make a block of text representing a storage area,
    #@ as appears in the hec-ras format .g01
    #@
    #@ e.g.
    #@
    #@ Storage Area=east_bank1      ,510988.0387824,1620987.9181366
    #@ Storage Area Surface Line= 15 
    #@  510643.523093591621316.98005342                
    #@ 510548.0930744711621305.29474495                
    #@  510709.739841551621272.18637097                
    #@ 510723.3727014251621141.70042646                
    #@ 510738.9531127091620997.58162208                
    #@ 510735.0580098881620913.83691142                
    #@ 510700.0020844971620832.03975218                
    #@ 510672.7363647491620759.98034998                
    #@ 510657.1559534641620682.07829356                
    #@ 511175.2046286831620545.74969482                
    #@ 511336.8513957621620814.51178948                
    #@ 511453.7044803981621168.96614621                
    #@ 511299.8479189611621264.39616533                
    #@ 510898.6523283781621262.44861392                
    #@ 510666.8937105171621313.08495059                
    #@ Storage Area Type= 1 
    #@ Storage Area Area=
    #@ Storage Area Min Elev=
    #@ Storage Area Vol Elev= 84 
    #@    17.35       0   17.45    .005   17.55    .025   17.65    .075   17.75    .185
    #@    17.85.3600001   17.95.6250002   18.051.019999   18.15   1.595   18.252.375064
    #@    18.353.335002   18.454.444993   18.555.669004   18.656.999982   18.758.465041
    #@    18.8510.15023   18.95   12.17   19.0514.55502   19.1517.26011   19.2520.21989
    #@    19.3523.39999   19.4526.79979   19.5530.42019   19.6534.26412   19.75  38.299
    #@    19.85   42.64   19.9547.40543   20.0552.62051   20.1558.45985   20.25 65.0401
    #@    20.3572.31497   20.4580.16493   20.5588.45006   20.6597.16591   20.75106.3653
    #@    20.85116.1453   20.95126.5704   21.05137.5099   21.15 148.912   21.25160.7833
    #@    21.35173.0752   21.45 185.787   21.55198.9568   21.65212.6048   21.75226.5446
    #@    21.85240.7119   21.95255.1156   22.05269.7048   22.15 284.454   22.25299.3548
    #@    22.35314.3996   22.45329.6109   22.55344.9856   22.65360.5155   22.75376.2579
    #@    22.85392.1823   22.95408.4679   23.05424.5935   23.15 441.215   23.25458.2499
    #@    23.35475.6863   23.45493.6751   23.55512.3254   23.65532.1439   23.75553.4506
    #@    23.85577.0807   23.95604.1621   24.05634.8083   24.15668.5247   24.25 704.508
    #@    24.35741.9367   24.45780.8355   24.55 820.769   24.65861.5136   24.75904.3318
    #@    24.85944.7191   24.95986.9531   25.051029.436   25.151072.018   25.251114.711
    #@    25.351157.568   25.451200.513   25.551243.467   110.537729.49
    #@
    #@
    #@ 
    #@    These string manipulations can be pretty hacky
    #@
    if(length(storage_area)!=1) stop('Length of storage area must =1 in make_storage_area_text')
    require(rgeos)

    # Try to filter storage area coordinates -- we sometimes have way too many points
    storage_area=gSimplify(storage_area,tol=glob_vars$storage_poly_simplify_tol,
                           topologyPreserve=TRUE)

    #@ Compute coordinates inside poly
    storage_area_central_coords=coordinates(storage_area)

    #@ Compute bounding coordinates -- got to love the notation
    storage_area_bounding_coords=coordinates(gBoundary(storage_area))[1][[1]][[1]]

    #@ Remove final bounding coordinate (which is a repeat of the first point,
    #@ and does not feature in hec)
    l=length(storage_area_bounding_coords[,1])
    storage_area_bounding_coords=storage_area_bounding_coords[-l,]

    ## TRY TO FILTER storage_area_bounding_coords as stored in hec-ras
    ## Because it can be too large often, perhaps due to the details of the spatial processing
    ## Hopefully not needed, as we simplify above
    if(length(storage_area_bounding_coords[,1])>glob_vars$thin_storage_poly_pointcount){
        print('Forcefully thinning storage area [still too many points after gSimplify]')
        usdist=c(0, cumsum(sqrt(diff(storage_area_bounding_coords[,1])**2 + 
                                diff(storage_area_bounding_coords[,2])**2)))
        x_fun=approxfun(usdist,storage_area_bounding_coords[,1])
        y_fun=approxfun(usdist,storage_area_bounding_coords[,2])

        lmax=max(usdist)
        # How many coordinates can we accept?
        # Max of max_storage_poly_pointcount
        # Min spacing = storage_poly_simplify_tol. 
        # And don't add any points
        num_coords=min(glob_vars$max_storage_poly_pointcount, 
                       floor(lmax/glob_vars$storage_poly_simplify_tol),
                       length(storage_area_bounding_coords[,1]))
        dist_seq=seq(0,lmax,len=num_coords)
        x_new=x_fun(dist_seq)
        y_new=y_fun(dist_seq)

        storage_area_bounding_coords=cbind(x_new,y_new)
    }

    #browser()
    #@ Start producing output text

    output_text = c() # Initialise output_text

    #@ Create character strings of correct format for name
    name_nonempty = as.character(name)
    name_fil = rep(" ", 16-nchar(name_nonempty))
    name_fil = paste(name_fil,collapse='')

    #@ Create coordinate character strings with correct format
    coord1 = format(as.character(signif(storage_area_central_coords[1,1],13)), width=15, justify='none', trim=T)
    coord2 = format(as.character(signif(storage_area_central_coords[1,2],13)), width=15, justify='none', trim=T)

    #@ Build first line of output text
    first_line = paste("Storage Area=", name_nonempty, name_fil, ",", coord1,",", coord2, sep="")
    #print(first_line)
    output_text = c(output_text, first_line)

    #@ Build the second line of output text
    second_line = paste('Storage Area Surface Line=', l-1)
    output_text = c(output_text,second_line)

    coord1 = pad_string(as.character(signif(storage_area_bounding_coords[,1],15)), 16, pad="0", justify='left') 
    coord2 = pad_string(as.character(signif(storage_area_bounding_coords[,2],15)), 16, pad="0", justify='left') 
    
    next_lines = paste(coord1,coord2,sep="")
    l = length(coord1)
    next_lines = paste(next_lines, rep(" ", l, collapse=""),sep="") 
    output_text = c(output_text,next_lines)

    #@ Add storage area type info
    output_text = c(output_text, c("Storage Area Type= 1", "Storage Area Area=","Storage Area Min Elev="))

    #browser()
    #@ Add volume - elev information
    l = length(elev_vol[,1])
    output_text = c(output_text,paste('Storage Area Vol Elev=', l) )
    elev_coord = pad_string(as.character(round(elev_vol[,1],2)), charlen=8, pad=" ", justify='right')
    vol_coord = pad_string(as.character(signif(elev_vol[,2],6)), charlen=8, pad=" ", justify='right')

    next_lines = paste(elev_coord,vol_coord,sep="")
    # Get the format right
    next_lines = format_in_rows(next_lines,5)

    output_text = c(output_text,next_lines)

    return(output_text)
    #print(output_text)
}


#########################################################################################

#' Make hecras text for a storage connection
#'
#' Make hecras text for a storage area connection, with lots of assumed parameters (careful).
#' Typically used to get a connection into the geometry, but then the parameter
#' values must be edited separately
#'
#' @param store1 SpatialPolygonsDataFrame of length(1) -- the first storage area to join
#' @param store2 The second storage area to join
#' @param name1 The name of the first storage area
#' @param name2 The name of the second storage area
#' @param lidar_DEM A raster digital elevation model, projected coordinate system, with res(lidar_DEM) in metres
#' @param vertical_datum_offset A constant to be added to the lidar_DEM values before processing
#' @param storage_area_overlap_merge_threshold If the storage areas overlap by > this fraction of the area of the smaller storage area, throw an error.
#' @return lots of text
#' @export
#' @examples
#'   require(raster)
#'   require(rgdal)
#'   require(rgeos)
#'   mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'   mydem=paste0(system.file(package='RgeoRAS'), '/extdata/10m_dem.tif')
#'   lidar_DEM=raster(mydem)
#'   myStoragePoly=spatial_storage_areas(mychanfile, CRS('+init=epsg:3123'))
#'   P1=myStoragePoly[1,]
#'   P2=myStoragePoly[536,]
#'   msct = make_storage_connection_text(P1, P2, 'Blah', 'Bloo', lidar_DEM, vertical_datum_offset=0)
#'   print(msct)

make_storage_connection_text<-function(store1, store2, name1, name2, lidar_DEM, 
                                        vertical_datum_offset=0, 
                                        storage_area_overlap_merge_threshold=glob_vars$storage_area_overlap_merge_threshold){
    #@
    #@ Function to develop the text for a storage area connection between 2 overlapping polygons, store1 & store2
    #@ It finds the hypsometry in their overlapping zone, and uses this to compute the geometry of their weir
    #@ The text is for insertion into a hec-ras .g01 file
    #@

    #@ The format looks like this
    #@
    #@ Connection=south_east_mont_,514493.3662094,1627706.738947
    #@ Connection Desc=south_east_mont_1_to_2
    #@ Connection Line= 3 
    #@ 514532.9213419731627758.28048332514532.9213419731627758.28048332
    #@ 514414.2559442781627603.65587421
    #@ Connection Last Edited Time=Jun/03/2012 19:27:42
    #@ Connection Up SA=south_east_mont1
    #@ Connection Dn SA=south_east_mont2
    #@ Conn Routing Type= 1 
    #@ Conn Use RC Family=True
    #@ Conn Weir WD=20
    #@ Conn Weir Coef=1.66
    #@ Conn Weir Is Ogee= 0 
    #@ Conn Simple Spill Pos Coef=0.05
    #@ Conn Simple Spill Neg Coef=0.05
    #@ Conn Weir SE= 2 
    #@        0      24     300      24
    #@ Conn HTab HWMax=40
    #@
    #@
    print(' ')
    print('#########################################')
    print(paste('Connecting ', name1, 'with', name2))
    print(paste('These are located near'))
    print(coordinates(store1))
    print(coordinates(store2))
    print('#########################################')
    print(' ')    
    intersection=gIntersection(store1, store2) # Polygon containing the intersection of the 2 storage areas
   
    #@ Check that they do not overlap too much -- could be a problem
    A1=gArea(store1)
    A2=gArea(store2)
    A3=gArea(intersection)
    
    overlap_max=A3/min(A1,A2)
    overlap_error_threshold=storage_area_overlap_merge_threshold
    if(overlap_max>overlap_error_threshold){
        if(A1>A2){
            plot(store1,axes=TRUE,asp=1)
            plot(store2,border='red',add=T)
            title('Storage areas which overlap too much')
        }else{
            plot(store2,axes=TRUE,asp=1)
            plot(store1,border='red',add=T)
            title('Storage areas which overlap too much')
        }
        print('')
        print('#################################################')
        stop(paste('ERROR: Storage areas', name1, 'and', name2, 'overlap by > ', 
                    overlap_error_threshold*100, '%. This sounds like an error', 
                    '- check your input data'))
    }
    
    output_text=c() # Predefine output

    #@
    #@ Compute coordinates for connection
    #@
    connection_pts=coordinates(intersection)
    #@ Format connection_pts
    con_x=pad_string(as.character(connection_pts[1]), 16,pad="0")
    con_y=pad_string(as.character(connection_pts[2]), 16,pad="0")
    
    #@
    #@ Make name for connection -- include a timestamp to make unique
    #@
    name_timestamp=as.character(floor(as.numeric(Sys.time())*100)%%100000000)
    #connection_name=paste('SC______',name_timestamp,sep="") 
    connection_name=paste(name1,name2,sep="")
    long_connection_name=paste('Connect', name1, '&', name2)

    first_line=paste('Connection=', connection_name,",", con_x,",", con_y,sep="")
    second_line=paste('Connection Desc=', long_connection_name, sep="")
    third_line= paste('Connection Line= 2')

    #@
    #@ now make up coordinates for connection line
    #@

    #@ Line direction = direction of line joining the 2 centroids
    store1_cent=coordinates(store1)
    store2_cent=coordinates(store2)
    vec=store1_cent-store2_cent
    vec=vec/10 # This vector will give us the length of the drawn storage area connection line
    p1=connection_pts+vec
    p2=connection_pts-vec
    #@ Format line points
    p1x=pad_string(as.character(p1[1]), 16, pad="0")
    p1y=pad_string(as.character(p1[2]), 16, pad="0")
    p2x=pad_string(as.character(p2[1]), 16, pad="0")
    p2y=pad_string(as.character(p2[2]), 16, pad="0")
    fourth_line=paste(p1x, p1y, p2x, p2y, sep="")
    
    #@
    #@ Now make the 'connection_edited' timestamp
    #@
    edit_time=format(Sys.time(), "%b/%d/%Y %H:%M:%S") # Timestamp like hec-ras
    fifth_line=paste("Connection Last Edited Time=", edit_time,sep="")

    output_text=c(first_line, second_line, third_line, fourth_line, fifth_line)

    #@
    #@ Make connection info
    #@
    next_lines=c(paste('Connection Up SA=', name1, sep=""), 
                 paste('Connection Dn SA=', name2, sep=""))

    output_text=c(output_text,next_lines)
  
    #@ 
    #@ Add other weir info
    #@ 
    next_lines=glob_vars$store_conn_weir_properties
    #             c("Conn Routing Type= 1",  
    #             "Conn Use RC Family=True",
    #             "Conn Weir WD=20",
    #             "Conn Weir Coef=1.66",
    #             "Conn Weir Is Ogee= 0", 
    #             "Conn Simple Spill Pos Coef=0.05",
    #             "Conn Simple Spill Neg Coef=0.05")
    
    output_text=c(output_text,next_lines) 


    #@
    #@ Compute weir form 
    #@
    
    #elev_relation=lidar_DEM[intersection][[1]]
    elev_relation=extract(lidar_DEM, intersection, small=TRUE)[[1]]
    #if(length(elev_relation)==0){
    #    print(' ')
    #    print('WARNING: Not creating a storage area connection for')
    #    print(paste(name1, ' to ', name2))
    #    print('Because the overlap is too small to enclose any points in the DEM')
    #    return(NA)
    #}
        
    elev_relation=elev_relation+ vertical_datum_offset
    elev_relation2=sort(elev_relation)
    #@ How to compute the weir length??
    #@ Idea:
    #@ gLength(intersection) = poly boundary length
    #@ If we assume poly is long and thin, then length~ = boundary length /2 
    #@ We could make it of slightly shorter length to be conservative (e.g. boundary_length *1/3 or *5/12)
    weir_length = gLength(intersection)*glob_vars$weir_length_scaling
   
    minimum_weir_length=glob_vars$min_weir_length 
    if(weir_length<minimum_weir_length){
        print(' ')
        print('WARNING: Not creating a storage area connection for')
        print(paste(name1, ' to ', name2))
        print('Because the computed weir length is small. It is')
        print(weir_length)
        return(NA)
    }
        
    l=length(elev_relation)
    weir_x_vals=seq(0,weir_length, len=l) # X values at which we will get weir elevation points
    l2=min(l,glob_vars$max_weir_points) # Number of points on the weir in hec-ras

    if(l>1){
        weir_relation=approx(weir_x_vals, elev_relation2, n=l2) # Weir x - elev relation
    }else{
        # Treat the case of only one point
        l2=2
        weir_relation=approx(c(0, weir_length), c(elev_relation2, elev_relation2),n=l2)
    }

    weir_x=pad_string(as.character(signif(weir_relation$x,7)), 8, pad=" ", justify='right')
    weir_y=pad_string(as.character(signif(weir_relation$y,7)), 8, pad=" ", justify='right')

    weir_text=paste(weir_x,weir_y,sep="")
    weir_text=format_in_rows(weir_text,5)
    output_text=c(output_text, paste("Conn Weir SE=", l2))
    output_text=c(output_text, weir_text)


    #@ Append the Htab HWMax parameter
    output_text=c(output_text, 
                  paste("Conn HTab HWMax=", 
                       round(max(elev_relation2)+glob_vars$weir_htab_hwmax_offset,2),sep=""))

    return(output_text)
}


##############################################################

#' Make lateral weir text from storage area / channel gis information
#'
#' For adding lateral weirs to hec-ras files, when storage areas are added with polygons
#'
#' @param storage_poly storage area polygon of length 1
#' @param storage_name name of storage area
#' @param chan_poly channel polygon from spatial_channel_polygon
#' @param chan_pts channel boundary points from spatial_channel_boundary_points
#' @param lidar_DEM Digital elevation model raster
#' @param vertical_datum_offset Number to add to lidar_DEM elevations 
#' @param hec_lines vector of character strings containing hecras geometry file
#' @param bridge_lines lines in hec_lines matching "Bridge Culvert"
#' @param limit_weir_elevation_by_channel_bank_elevation TRUE/FALSE -- place a (local) lower limit on a weir elevation
#' @param min_structure_elev [NOT IMPLEMENTED] another limit on the storage elevation
#' @param lower_limit_on_lateral_weir_elevations A global lower limit on the lateral weir elevations
#' @return modified version of hec_lines
#' @export

make_lateral_weir_text<-function(storage_poly, storage_name, chan_poly, chan_pts, lidar_DEM, 
                                 vertical_datum_offset=0, hec_lines, bridge_lines,
                                 limit_weir_elevation_by_channel_bank_elevation,
                                 min_structure_elev, lower_limit_on_lateral_weir_elevations){
    #@ Given a storage polygon and a channel polygon (which intersect), make me
    #@ a lateral weir and place it into hec_lines
    #@
    #@ Lateral weirs look like this:
    #@
    #@ Type RM Length L Ch R = 6 ,26250   ,,,
    #@ BEGIN DESCRIPTION:
    #@ south_east_mont2_to_channel
    #@ END DESCRIPTION:
    #@ Node Last Edited Time=May/04/2012 13:57:15
    #@ Lateral Weir Pos= 0 
    #@ Lateral Weir End=                ,                ,        ,south_east_mont2
    #@ Lateral Weir Distance=10
    #@ Lateral Weir TW Multiple XS=-1
    #@ Lateral Weir WD=20
    #@ Lateral Weir Coef=1.1
    #@ Lateral Weir WSCriteria=-1 
    #@ Lateral Weir Flap Gates= 0 
    #@ Lateral Weir Hagers EQN= 0 ,,,,,
    #@ Lateral Weir SS=0.05,0.05,
    #@ Lateral Weir Type= 0 
    #@ Lateral Weir Connection Pos and Dist= 0 ,
    #@ Lateral Weir SE= 2 
    #@        0    24.5    1050    24.5
    #@ Lateral Weir HW RS Station=26275.4*,-10
    #@ Lateral Weir TW RS Station=,0
    #@ LW Div RC= 0 ,False,
    #@
    #@

    print('###')
    print(' ')
    print(paste('WARNING: Using channel downstream distances: Consult RAS',
                'Manual as to whether the bank distances should be used instead'))
    print(' ')
    print('###')

    #@ Compute intersection of storage poly with chan poly
    intersection=gIntersection(storage_poly,chan_poly)
    
    #@ Make sure that intersection contains only 1 connected polygon
    if(length(intersection@polygons[[1]]@Polygons)>1){
       #@ The intersection consists of > 1 disjoint polygon. Choose the one with largest area
       areas=lapply(intersection@polygons[[1]]@Polygons, getarea<-function(x) x@area) 
       areas=unlist(areas)
       tmp=which.max(areas) # Index of one with largest area
       tmp_Polygons=Polygons(list(intersection@polygons[[1]]@Polygons[[tmp]]), ID=1)
       intersection=SpatialPolygons(list(tmp_Polygons), proj4string=CRS(proj4string(storage_poly)))
    }
  
    #@ Get channel boundary points 
    vv=over(chan_pts, intersection)
    vv=which(!is.na(vv))
    if(length(vv)==0){
        print('##')
        print(paste('WARNING: There is an intersection of', storage_name, ' with the channel.'))
        print('However, it does not contain any bank points')
        print('Skipping this one')    
        print(' ')
        return(hec_lines)
    }
    weir_pts=chan_pts[intersection,]

    #@ If there are no points, return
    if(length(weir_pts)==0){
        return(hec_lines)
    }

    #@ Check to ensure we intersect the channel on only 1 bank
    banks=weir_pts$bank
    bank2=union(banks,banks)
    if(length(bank2)>1){
        print('####################')
        print(paste('WARNING: Storage polygon,', storage_name, "intersects on both the left and right banks"))
        print(paste('It is located near'))
        print(coordinates(weir_pts[1,]))
        num_left=sum(weir_pts$bank=='L')
        num_right=sum(weir_pts$bank=='R')
        print(paste('There are', num_left, 'points on the left bank, and', num_right, 'points on the right bank'))
        if(num_left>num_right){
            print('Using only points on the left bank')
            weir_pts=weir_pts[weir_pts$bank=='L',]
            bank2='L'
        }else{
            print('Using only points on the right bank')
            weir_pts=weir_pts[weir_pts$bank=='R',]
            bank2='R'
        }
        print(' ')
    }
    
    ##@ Check to ensure that the weir_pts do not include a bridge.
    ##@ Hecras cannot have lateral structure spanning a bridge
    weir_lines=weir_pts@data$line_num # Line numbers associated with the weir_lines xsections
    upstream_bridges=which(bridge_lines<min(weir_lines))
    downstream_bridges=which(bridge_lines> max(weir_lines))
    if(length(upstream_bridges)+length(downstream_bridges) < length(bridge_lines)){
        # Check that the weir points are sorted (they should be)
        if(min(diff(weir_lines))<0){
            stop('ERROR: Found unsorted weir_pts which apparently intersect a bridge. Need to code this case')
        }

        # Categorise weir_pts into groups without internal bridges
        weir_cut = cut(weir_lines, bridge_lines) 
        weir_sublen= sort(table(weir_cut), decreasing=TRUE)
        # Extract the most commonly occuring points
        longest_weir=which(weir_cut==as.factor(names(weir_sublen))[1])
        # Update the weir pts
        weir_pts=weir_pts[longest_weir,]
        lwp=length(weir_pts)
        if(lwp>2) weir_pts=weir_pts[2:(lwp-1),] # 
        
        print('Shortening lateral weir due to bridge near:')
        print(weir_pts[1,])

    }
    
    # Don't allow more than "max_latweir_chanpts" channel-bank-points to be in the weir
    if(length(weir_pts[,1])>glob_vars$max_latweir_chanpts){
        print('Shortening lateral weir due to too many points, near:')
        print(weir_pts[1,])
        weir_pts=weir_pts[1:glob_vars$max_latweir_chanpts,]
    }
    
    
    #@ Sort the bank stations
    st1=as.character(weir_pts$station_name)
    st2=gsub('\\*', '', st1) # Remove * symbol
    st3=as.numeric(st2) # Now they are numbers

    #@ Sort the weir points along the channel
    station_order=sort(st3,index.return=T, decreasing=T)

    #@ Remove start and end points -- hec will not accept connections at start/end of channel
    if(length(station_order$ix)>2){
        SINGLE_POINT_FLAG=0 #The weir covers multiple cross-sections
    }else{
        print('##')
        print(paste('WARNING: There is an intersection of', storage_name, ' with the channel.'))
        print('However, it does not contain more than 2 bank points')
        print('I will try to add a single-point lateral weir instead')
        print(' ')
        SINGLE_POINT_FLAG=1 # The weir covers at most 1 cross-section.
    }

    tmp_coord=as.character(coordinates(weir_pts)[1,])
    tmp_name=weir_pts$reach_name[1]
    tmp_stat=weir_pts$station_name[1]
    print(' ')
    print('##############################')
    print(paste('Connecting ', storage_name, ' to the river', tmp_name, 'at station', tmp_stat, ' near:'))
    print(tmp_coord)
    print('##############################')
    print(' ') 
    #@ Get coordinates of the weir points, and downstream distances, and make a
    #@ line along them with 3 times as many points. We will get weir elevations
    #@ along this line
    weir_line = coordinates(weir_pts)
    ll=length(weir_line[,1])
    if(ll<2){
        print(' ')
        print('#####################################################')
        print('WARNING: Storage area intersects channel at only one point') 
        print('The relevant point is:')
        print(weir_pts)
        print('No lateral weir will be made here')
        print('#####################################################')
        print(' ')
        return(hec_lines)
    }
    interpolated_length=min(ll+2,glob_vars$max_latweir_chan2storage_pointcount)
    weir_line=cbind(weir_line, c(0, cumsum(weir_pts$downstream_distance[1:(ll-1)])), weir_pts$bank_elev) # Append distance and bank elevation

    # Slightly shorten the downstream distances of weir line: This is a hack to
    # prevent slight overshoots of the weir line beyond the downstream cross-section, which occur for other reasons.
    if(max(weir_line[,3])>0.){
        weir_line[,3] = weir_line[,3]/(max(weir_line[,3]))*(max(weir_line[,3])-glob_vars$weir_line_trim)
        if(max(weir_line[,3])<0.0) stop('ERROR: Weir lines are now too short')
    }
    weir_line_xint = approx(weir_line[,3], weir_line[,1], n=interpolated_length) # Interpolate x's
    weir_line_yint = approx(weir_line[,3], weir_line[,2], n=interpolated_length) # Interpolate y's
    weir_line_bank_elev_limit = approx(weir_line[,3], weir_line[,4], n=interpolated_length) # Interpolate the bank elevation

    #@ weir_line = downstream_distance, x_coordinate, y_coordinate
    weir_line=cbind(weir_line_xint$x, weir_line_xint$y, weir_line_yint$y, weir_line_bank_elev_limit$y)

    #@ Sample the raster elevations at points on weir_line
    transect_inds = cbind(rowFromY(lidar_DEM, weir_line[,3]), colFromX(lidar_DEM, weir_line[,2]) )
    weir_elev=lidar_DEM[transect_inds]
    weir_elev=weir_elev + vertical_datum_offset

    if(limit_weir_elevation_by_channel_bank_elevation){
        # Make sure that the weir elevation is >= the channel bank elevation
        weir_elev=pmax(weir_elev, weir_line[,4])
    }

    # Ensure that min weir elev is not below the lower_limit_on_lateral_weir_elevations
    if(lower_limit_on_lateral_weir_elevations!=-Inf){
        print(paste('CAREFUL: Forcing the lateral weir elevations to be > lower_limit_on_lateral_weir_elevations ( ', 
                     lower_limit_on_lateral_weir_elevations, ')'))
        weir_elev=pmax(weir_elev, lower_limit_on_lateral_weir_elevations)
    } 
        
    weir_relation=cbind(weir_line[,1], weir_elev)
   
    #@
    #@ Develop text information for write out modified hec_lines
    #@ 

    #@ Define weir station name, just downstream of the most upstream bank station
    #@ Use a different distance for left and right bank points -- trick to
    #@ avoid weirs on both banks with the same station name
    lateral_weir_distance_name=(0.25 + 0.5*(bank2=='R'))*(station_order$x[1]-station_order$x[2])
    #lateral_weir_distance=(0.25 + 0.5*(bank2=='R')) + min(1.0,(station_order$x[1]-station_order$x[2]))
    weir_station_name=station_order$x[1]- lateral_weir_distance_name

    output_text=c()
    line1= paste("Type RM Length L Ch R = 6 ,", round(weir_station_name,3), ",,, ", sep="")
    output_text=c(output_text,line1)
    output_text=c(output_text, 'BEGIN DESCRIPTION:')
    output_text=c(output_text, paste(storage_name, '_to_chan', sep=""))
    output_text=c(output_text, 'END DESCRIPTION:')

    #@ Add timestamp
    edit_time=format(Sys.time(), "%b/%d/%Y %H:%M:%S") # Timestamp like hec-ras
    output_text=c(output_text, paste("Node Last Edited Time=", edit_time,sep=""))

    bankflag=0 + 3*(bank2=='R') # 0 for left bank, 3 for right bank
    output_text=c(output_text, paste("Lateral Weir Pos= ",bankflag, sep=""))


    output_text=c(output_text,
                  paste("Lateral Weir End=                ,                ,        ,",storage_name,sep=""))

    #output_text=c(output_text, paste('Lateral Weir Distance=', round(lateral_weir_distance,3),sep=""))
    output_text=c(output_text, 
                    paste('Lateral Weir Distance=', 
                          round(glob_vars$lat_weir_downstream_distance,3),sep="")) # Make 1m downstream of upstream section

    if(SINGLE_POINT_FLAG==0){
        lw_tw_text="Lateral Weir TW Multiple XS=-1"
    }else{
        lw_tw_text="Lateral Weir TW Multiple XS=0"
    }
    next_lines=c(lw_tw_text,glob_vars$lat_weir_properties)

    output_text=c(output_text,next_lines)
    output_text=c(output_text, paste("Lateral Weir SE= ",interpolated_length,sep=""))

    #@ Add weir distance-elevation information
    weir_x=pad_string(as.character(round(weir_relation[,1],3)), 8, pad=" ", justify='right')
    weir_y=pad_string(as.character(round(weir_relation[,2],3)), 8, pad=" ", justify='right')
    weir_text=paste(weir_x,weir_y,sep="")
    weir_text=format_in_rows(weir_text,5)
    output_text=c(output_text, weir_text)

    upstream_station=pad_string(as.character(station_order$x[1]), 8, pad=" ", justify='left')

    output_text=c(output_text, 
                  paste("Lateral Weir HW RS Station=", 
                         upstream_station,",", 
                         -round(lateral_weir_distance_name,3),sep=""))

    output_text=c(output_text, "Lateral Weir TW RS Station=,0")
    output_text=c(output_text, "LW Div RC= 0 ,False,")

    #@
    #@
    #@ Now find the line in 'hec_lines' where we need to insert this weir
    #@ Note that we will be altering hec_lines with every call to this function
    #@ So we need to recompute the locations
    #@


    #@ Identify the start of the channel of interest
    chan_ind=grep('River Reach=', hec_lines) # All channels
    my_chan_ind=grep(weir_pts$reach_name[1], hec_lines[chan_ind]) # Should only match 1

    #@ Compute lower bound of 'lines of interest'
    my_lower_ind=chan_ind[my_chan_ind]
    #@ Compute upper bound of 'lines of interest'
    ll=length(hec_lines)
    if(my_chan_ind<length(chan_ind)){
        my_upper_ind=chan_ind[my_chan_ind+1]
    }else{
        my_upper_ind=ll
    }
  
    #@ Find the line at the start of the upstream station
    line_pattern = paste('Type RM Length L Ch R = 1 ,', weir_pts$station_name[1], sep="")
    #@ NOTE: Match can contain a * -- need to use 'fixed=TRUE' to get this
    upstream_station_ind = grep(line_pattern, hec_lines[my_lower_ind:my_upper_ind], fixed=TRUE) + my_lower_ind-1

    #@ Find next blank line (search next 300 lines). Identified because it has
    #@ no letters or numbers
    upper_search_ind = min(upstream_station_ind+glob_vars$next_station_linesearch, ll)
    blank_loc = which(hec_lines[upstream_station_ind:upper_search_ind]%in%c("", " "))[1] + upstream_station_ind-1
    hec_linestmp = c(hec_lines[1:blank_loc], output_text, " ", hec_lines[(blank_loc+1):ll])

    return(hec_linestmp)
}

#################################################################################################################

#' Make hecras geometry text for a cross-section
#'
#' @param station_name -- the name of the station (at most 8 characters)
#' @param section_cutline -- 2-column matrix with x-y coordinates defing the spatial locations of the cross-section 
#' @param section -- 2-column matrix with distance-elevation coordinates defining the cross-section profile
#' @param downstream_distance -- the downstream distance of the cross-section. Can be a vector of length 3, with values for the left bank,channel,right bank. Will be rounded to 2 decimal places.
#' @param mann -- (not implemented) manning's n value
#' @param HTAB_increm -- increment for the HTAB parameters
#' @return text defining the cross-section that can be inserted in a hecras geometry file
#' @export
#' @examples
#'  require(raster)
#'  # Read base data
#'  mydem=paste0(system.file(package='RgeoRAS'), '/extdata/10m_dem.tif')
#'  lidar_DEM=raster(mydem)
#'  
#'  # Make an artificial section cutline  (would typically use real data with many more points)
#'  section_cutline=matrix(c(509619.4, 511972.1, 1610852, 1613042), ncol=2)
#'  len_section_cutline= sqrt(diff(section_cutline[,1])**2 + diff(section_cutline[,2])**2)
#'  # Make an artificual section profile along section cutline
#'  section=matrix(c(seq(0,len_section_cutline,len=4), seq(-20,-20,len=4)),ncol=2)
#'  
#'  # Apply the function
#'  newsection=compute_crossSection_elev_with_dem(section,section_cutline, lidar_DEM)
#'  outtext=make_crossSection_text('100.102', section_cutline,
#'                     newsection, downstream_distance=c(195,200.0,197))
#'  print(outtext)
make_crossSection_text<-function(station_name, section_cutline, section, 
                                 downstream_distance, mann=0.03, HTAB_increm=0.2){
    #
    # Make hecras text for this cross-section
    #

    #if(!is.na(mann)) stop('Setting manning n not yet implemented -- but see edit_manning_from_shapefile')
  
    if(nchar(station_name)>8){
        print('Problem with')
        print(station_name)
        stop('station_name should have 8 or less characters (or it will be truncated, dangerous)')
    }

    if(length(downstream_distance)==1){
        downstream_distance=rep(downstream_distance,3)
    } 
    dd=as.character(round(downstream_distance, 2))
    output=paste("Type RM Length L Ch R = 1 ,", 
                 pad_string(station_name, 8, justify='left'), ',', 
                 dd[1], ',', dd[2], ',', dd[3], sep="")

    # Make section_cutline
    output=c(output, paste0('XS GIS Cut Line=', length(section_cutline[,1])))
    coord_text=pad_string(as.character(t(section_cutline)), 16, justify='right')
    coord_text=format_in_rows(coord_text,4)

    output=c(output, coord_text)

    output=c(output,"Node Last Edited Time=Jan/01/2013 00:00:01")

    #browser()
   
    # Construct lidar text 
    output=c(output, paste('#Sta/Elev= ', length(section[,1])))
    lidar_text=pad_string(as.character(t(round(section,2))), 8, justify='right')
    lidar_text=format_in_rows(lidar_text,10) 

    output=c(output, lidar_text)

    # Add dummy manning values
    output=c(output, "#Mann= 3 ,-1 , 0 ")
    mann_txt=pad_string(as.character(mann), 8, justify='right')
    zero_txt=pad_string(as.character(0), 8, justify='right')
    tmp=paste(pad_string(as.character(round(min(section[,1]),2)), 8, justify='right'), 
              mann_txt,zero_txt,sep="")
    tmp=c(tmp, paste(pad_string(as.character(round(median(section[,1]),2)), 8, justify='right'), 
                    mann_txt, zero_txt,sep="") )
    tmp=c(tmp, paste(pad_string(as.character(round(max(section[,1]),2)), 8, justify='right'), 
                    mann_txt, zero_txt,sep="") )
    #browser()
    tmp=paste(tmp[1], tmp[2], tmp[3],sep="")
    output=c(output,tmp)

    # Add Bank station
    output=c(output, paste("Bank Sta=",min(round(section[,1],2)),',', max(round(section[,1],2)),sep="") )

    output=c(output,'XS Rating Curve= 0 ,0')

    output=c(output, paste('XS HTab Starting El and Incr=',
                           round(min(section[,2])+glob_vars$htab_start_above_bed_min,2),
                           ',', HTAB_increm,',',glob_vars$htab_number_of_points)) 

    output=c(output, glob_vars$expansion_and_contraction)

    # Add final blank line
    output=c(output, " ")
    return(output)
}
