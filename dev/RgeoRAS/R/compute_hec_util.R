
#########################################################################

#'  Compute a stage-volume relation for a polygon from a DEM
#'
#'  Function to compute the stage-volume relation for a region defined by the polygon my_poly
#'  , on a given lidar_Dem raster  
#'
#'  @param my_poly A SpatialPolygons or SpatialPolygonsDataFrame object with length(my_poly)==1
#'  @param lidar_DEM, A rasterLayer digital elevation model in a projected coordinate system. res(lidar_DEM) is in units of metres
#'  @param vertical_datum_offset A constant that is added to the lidar_DEM values before output
#'  @param upper_bound_stage_offset The maximal stage in the returned stage-volume
#'    curve will be max(elevation_in_mypoly)+upper_bound_stage_offset. Set it to ensure the highest possible water surface
#'    elevation in the storage area is not exceeded
#'  @return A 2-column matrix with a stage-Volume relation. Compared with the DEM, with the stage values offset by + vertical_datum_offset
#'  @export
#'  @examples
#'     require(raster)
#'     require(rgdal)
#'     mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'     mydem=paste0(system.file(package='RgeoRAS'), '/extdata/10m_dem.tif')
#'     lidar_DEM=raster(mydem)
#'     myStoragePoly=spatial_storage_areas(mychanfile, CRS('+init=epsg:3123'))
#'     myStageVolRel=compute_storage_stage_vol_relation(myStoragePoly[1,], lidar_DEM)
#'     plot(myStageVolRel,xlab='Stage',ylab='Volume')
#'     
#' 
compute_storage_stage_vol_relation<-function(my_poly,lidar_DEM, vertical_datum_offset=0, upper_bound_stage_offset=100){
    require(raster)
    require(rgdal)

    if(length(my_poly)!=1) stop('length(my_poly) must = 1 in compute_storage_stage_vol_relation')

    # Area of a pixel -- units should match the units of the output data.
    cell_area = xres(lidar_DEM)*yres(lidar_DEM)/1000. 
    #@ In hec-ras, the storage area units are in 1000s of m^3 -- so we need 10
    #@ lots of 10*10 squares filled in a metre of water to account for one unit.
    #@ Hence the strange use of units above

    #elev_pts=lidar_DEM[my_poly] # Elevation points inside the polygon
    elev_pts = extract(lidar_DEM, my_poly, small=TRUE)

    # Treat datum offset
    if(length(elev_pts) == 1){
        elev_pts[[1]] = elev_pts[[1]] + vertical_datum_offset
    }else{
        stop('ERROR: elev_pts has length > 1. Should never happen')
    } 
   
    #@ Number of points on the stage - volume curve. 
    #@ Ensure 5<= number of points <= 60. Ideally, only give a new point every 0.33 m 
    num_stagevol_points = min(30, max(5, (max(elev_pts[[1]])-min(elev_pts[[1]]) )*3 ) ) 

    elev_hist = hist(elev_pts[[1]] , n=num_stagevol_points)

    # Compute a sequence of stages at which we will evaluate the stored volume
    lower_bound = elev_hist$mids[1]-0.1 #
    # MAX value at which we want the volume. This is a trick so that hec-ras
    # never exceeds the range
    upper_bound = upper_bound_stage_offset + max(elev_pts[[1]]) 
    desired_stage = c(lower_bound, elev_hist$mids, upper_bound)
    area_fun = approxfun(desired_stage, 
                         c(0, cumsum(elev_hist$counts)*cell_area, sum(elev_hist$counts)*cell_area), 
                         rule=2 )

    # Function to compute volume (note: volume = integral (Area))
    vol_fun<-function(stage){
        integral=integrate(area_fun, lower_bound, stage,subdivisions=1000)
        integral$value
    }

    output = cbind(desired_stage, sapply(desired_stage,vol_fun))

    # Check that the volume increases sufficiently between consecutive points
    # If it does not, replace with average of upper + lower (equivalent to
    # removing the point)
    # Otherwise rounding in hecras can lead to a non-increasing stage-volume
    # curve, and hecras error
    # FIXME: Consider better method for selecting replacement points
    dv = diff(output[,2])
    de = diff(output[,1])
    lo = length(output[,2])
    aves = c(output[1,2], 0.5*(output[3:lo,2] + output[1:(lo-2),2]), output[lo,2])
    increase_threshold = 0.01*de
    toreplace = which(dv < 0.01) 
    output[toreplace + 1,2] = aves[toreplace + 1]
    

    return(output)
}

#########################################################################################

#' Update elevation values on a cross-section using a DEM
#'
#' Given a cross-section defined by a profile and a spatial 'cutline', replace
#' the elevations with elevations from a DEM.
#' Optionally offset the DEM, and do not update points which fall inside a
#' given polygon shapefile, use the old points/compute new ones, ...
#'
#' @param section A 2-column matrix with the 'distance-elevation' points on the
#'              cross-section profile. The first point occurs at the start of section_cutline,
#'              and the last point at the end of section_cutline.
#' @param section_cutline A 2-column matrix with the (georeferenced) 'x-y'
#'         points defining the cross-section where the profile is measured. It
#'         does not have the same number of points as 'section', but the start and end points
#'         must coincide, and physically they cover the same transect.
#' @param dem A rasterLayer containing the DEM from which we will extract
#'            the new xsection elevations
#' @param dem_vertical_offset A number. If provided, this is added to the extracted dem
#'              values before they are returned (to deal with vertical datum offsets)
#' @param water_shp A SpatialPolygons/SpatialPolygonsDataFrame where we do NOT update the cross-section
#'              elevation values. A typical use of this would be to exclude areas where the DEM
#'              is known to be inaccurate (e.g. under-water areas for much LIDAR data)
#' @param check_watershp TRUE/FALSE Optionally, do not exclude section updates in water_shp, even if it is provided.
#' @param section_alignment_tol If the lengths of section_cutline and section[,1] differ by more than this, throw an error
#' @param compute_new_points TRUE/FALSE: If true, space the new xsection points
#'                                       evenly at desired_spacing (or larger if required to not exceed max_section_pts)
#' @param desired_spacing The desired (even) spacing of the new xsection points
#' @param max_section_pts The maximum number of xsectional points on the output cross-section
#'
#' @return A 2-column matrix with the new distance-elevation points. 
#' @details WARNING: Hec-ras does not allow the extreme points on the
#'              cross-section to be lower than their neighbouring point, and the code will
#'              ensure that this constraint is met. This can be seen in the example.
#' @export
#' @examples
#'  require(raster)
#'  require(rgdal)
#'  require(rgeos)
#'  
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
#'  plot(newsection,t='o',pch='.')
#'  
#'  # An example where we keep the points in the original section.
#'  newsection_test=compute_crossSection_elev_with_dem(section,section_cutline, 
#'                                      lidar_DEM, compute_new_points=FALSE)
#'  points(newsection_test,t='o',col='red',pch='.')
#'  
#'  # Another example where we keep the section points
#'  section2=approx(section[,1],section[,2],n=30)
#'  section2=cbind(section2$x,section2$y)
#'  newsection2=compute_crossSection_elev_with_dem(section2,section_cutline, 
#'                                        lidar_DEM, compute_new_points=FALSE)
#'  points(newsection2,t='o',col='blue',pch='.')
#'
#'  # Suppose there is a region where we don't want to use the DEM
#'  Exclude_DEM_Poly=SpatialPolygons(list(Polygons(list(Polygon(
#'                   matrix(c(510414.0, 509977.8, 509977.8, 512158.9, 512740.5, 510414.0,
#'                             1612569, 1612272, 1612272, 1608996, 1609890, 1612569),ncol=2)
#'                   )),ID='0')),proj4string=CRS(proj4string(lidar_DEM)))
#'  newsection_partial=compute_crossSection_elev_with_dem(section,section_cutline, 
#'                                  lidar_DEM, water_shp=Exclude_DEM_Poly)
#'  points(newsection_partial,t='l',col='orange')
#'   
#'  # Test that it also works for class(water_shp)=SpatialPolygonsDataFrame
#'  newsection_partial2=compute_crossSection_elev_with_dem(section,section_cutline, 
#'                             lidar_DEM, water_shp=as(Exclude_DEM_Poly,"SpatialPolygonsDataFrame"))
#'  legend('topleft', c('Fine', 'Very Coarse', 'Medium Coarse', 'Partial exclusion of DEM'),
#'         lty=rep(1,4),col=c('black','red','blue', 'orange'))
#'  title(main='Cross-sections with various point densities')
#'  title(sub='Notice how xsection end points are forced to be higher than their neighbours')
#' 
compute_crossSection_elev_with_dem<-function(section, section_cutline, 
                                 dem, dem_vertical_offset=0, 
                                 water_shp=NULL, check_watershp=!is.null(water_shp),
                                 section_alignment_tol=glob_vars$section_alignment_tol, 
                                 compute_new_points=TRUE, 
                                 desired_spacing=1.0, max_section_pts=400){
    # Take the cross-section with x-z points section, and spatial location 'cutline'  
    # Then output the section data with the lidar dem elevation + dem_vertical_offset
    # used in place of the original data where appropriate (i.e. outside of water_shp if check_watershp=TRUE).
    #browser()

    cutline_length=c(0, cumsum( ( diff(section_cutline[,1])**2 + diff(section_cutline[,2])**2)**0.5))
    aa = max(cutline_length)
    bb = diff(range(section[,1]))
    if(abs(aa-bb)>section_alignment_tol){
        print('############# section ################')
        print(section)
        print('############# section cutline ################')
        print(section_cutline)
        print(paste('The lengths of "section" and "section_cutline" differ by more than', section_alignment_tol))
        print('This indicates georeferencing errors, which if significant will break this routine')
        print('Either fix the spatial data, or if the error is acceptable, increase section_alignment_tol appropriately')
        stop()
    }

    if(desired_spacing>(max(cutline_length)/3)){
        stop('Desired spacing > one-third cutline length. This seems too large')
    }

    if(compute_new_points){
        # Compute points where new data will be sampled
        # Use a maximum of 400 points
        new_section_cutline_x=approx(cutline_length, section_cutline[,1], 
                                     n=min(ceiling(max(cutline_length)/desired_spacing), max_section_pts))
        new_section_cutline_y=approx(cutline_length, section_cutline[,2], 
                                     n=min(ceiling(max(cutline_length)/desired_spacing), max_section_pts))
        new_section_cutline=cbind(new_section_cutline_x$y, new_section_cutline_y$y)
    }else{
        if(length(section[,1])<=3){
            print('There are 3 or fewer points on this section')
            print('You will not get a good interpolation result (due to other restrictions on hec-ras cross-sections)')
            stop()

        }
        # Use the old points on the cross-section
        new_section_cutline_x=approx(cutline_length, section_cutline[,1], xout=section[,1],rule=2)
        new_section_cutline_y=approx(cutline_length, section_cutline[,2], xout=section[,1],rule=2)
        new_section_cutline=cbind(new_section_cutline_x$y, new_section_cutline_y$y)
    }

    # Compute the cross-channel coordinate for the new data
    new_section_cutline_length=new_section_cutline_y$x 
    new_section_x=new_section_cutline_length + section[1,1]

    # Extract the elevation data from the lidar at the appropriate locations
    rast_ys=rowFromY(dem,new_section_cutline[,2])
    rast_xs=colFromX(dem,new_section_cutline[,1])
    new_section_zs= dem[cbind(rast_ys, rast_xs)] + dem_vertical_offset

    ## NOTE - HEC-ras won't accept cross-section end-points which decrease -- so here we artificially prevent this
    npoints=length(new_section_zs)
    if(new_section_zs[1]<new_section_zs[2]) new_section_zs[1]=new_section_zs[2]+0.001
    if(new_section_zs[npoints]<new_section_zs[npoints-1]) new_section_zs[npoints]=new_section_zs[npoints-1]+0.001


    new_section=cbind(new_section_x, new_section_zs)

    # NOW, we might only accept new_section points which are not in the water_shp
    # Note: This can add to the computational expense
    if(check_watershp){
        # Compute old_section values along the new_section
        #browser()
        old_section=approx(section[,1], section[,2], xout=new_section_x, rule=2) 
        old_section=cbind(old_section$x, old_section$y)
        # WARNING -- We assume old_section and new_section line up perfectly in
        # plan view. However, there are potential for errors if the length of the
        # cutline and the length of the cross-channel coordinate are not the same.
        # This does happen sometimes, because the inputs are apparently not
        # perfect. Need to check visually

        new_section_cutline=SpatialPoints(new_section_cutline, proj4string=CRS(proj4string(water_shp)))

        # Compute indices where we use the old elevation values
        water_shp=as(water_shp,'SpatialPolygons') # Get around 'over' different behaviour for SP and SPDF
        use_old_tmp=over(new_section_cutline, water_shp) #
        use_old=1-is.na(use_old_tmp) 

        new_section[,2] = new_section[,2]*(1-use_old) + old_section[,2]*(use_old)
    }

    # FINALLY, compare the lengths of the cutline and the profile. They may still slightly differ
    # for multi-segment cutlines, because we won't necessarily have points exactly on the cutline vertices
    t1=max(new_section[,1])-min(new_section[,1])
    t2=max(cutline_length)

    if(abs(t1-t2) > glob_vars$section_alignment_tol){
        stop(paste('Profile and cutline lengths differ by more than ', 
                    glob_vars$section_alignment_tol,  '. This should not be!'))
    }

    return(new_section)
}


################################################################################

#' Compute the upstream distance of a point on a line
#'
#' Compute the upstream distance of point='point_coords'=c(x0,y0), along a line
#' defined by 'line_coords' = matrix with 2 columns
#
#' Also, compute the index of the segment in line_coords that point_coords
#' intersects with. This was most convenient to calculate here, even though
#' its a bit out of place 
#
#' point_coords should lie on line_coords to within a small tolerence. This
#' tolerance is is related to roundoff_tol in a complex fashion -- read the
#' source if you need to know!
#
#' NOTE: line_coords is assumed to be ordered from upstream to downstream.
#' Upstream distance is measured from the most downstream point in
#' line_coords
#'
#' @param point_coords  vector of length 2 with point coordinates
#' @param line_coords matrix with 2 columns defining the line. ASSUMES THAT FIRST POINT IS MOST UPSTREAM
#' @param roundoff_tol = Tolerence used to check if point is on the line
#' @return The upstream distance
#' @export

usdistfun<-function(point_coords, line_coords, roundoff_tol=1.0e-03){
    # Compute the upstream distance of point='point_coords'=c(x0,y0), along a line
    # defined by 'line_coords' = matrix with 2 columns
    #
    # Also, compute the index of the segment in line_coords that point_coords
    # intersects with. This was most convenient to calculate here, even though
    # its a bit out of place 
    #
    # point_coords should lie on line_coords to within a small tolerence. This
    # tolerance is is related to roundoff_tol in a complex fashion -- read the
    # source if you need to know!
    #
    # NOTE: line_coords is assumed to be ordered from upstream to downstream.
    # Upstream distance is measured from the most downstream point in
    # line_coords
     
    x0=point_coords[1]
    y0=point_coords[2]

    if(length(line_coords[,1])<2) stop('ERROR: line_coords has < 2 points')
    if(length(point_coords)!=2) stop('ERROR: point_coords does not describe a single point')

    # Compute upstream distance along line
    line_coords_rev=line_coords[length(line_coords[,1]):1,]
    us_dist=cumsum( (diff(line_coords_rev[,1])**2 + diff(line_coords_rev[,2])**2)**0.5)
    us_dist=c(0, us_dist)
    us_dist=rev(us_dist) # Indices follow those in line_coords

    output=NA
    connecting_segment=NA
    for(k in 1:(length(us_dist)-1)){
        # Check if x,y lies on this segment of us_dist
        # Do this by computing the change in x and y for 1 unit of movement
        # along the segment, and then taking a segment to x0,y0, computing
        # the same, and comparing
        ds = us_dist[k]-us_dist[k+1]  # This will be positive   
        dy=(line_coords[k+1,2]-line_coords[k,2])/ds # delta y
        dx=(line_coords[k+1,1]-line_coords[k,1])/ds # delta y

        ds2 = (( line_coords[k+1,2]-y0)**2 + (line_coords[k+1,1]-x0)**2)**0.5

        # Quick check to see if we are on possibly on this segment
        if( ds2>ds+roundoff_tol){
            # x0, y0 is not on the segment
            next
        }
        # Allow for degenerate case -- avoid division by ds2
        if(ds2<roundoff_tol){
            output=us_dist[k+1]
            connecting_segment=k
            break
        }

        dy2 = (line_coords[k+1,2]-y0)/ds2
        dx2 = (line_coords[k+1,1]-x0)/ds2

        # Test for equity, allowing for floating point error if ds2 is very small
        if((abs(dy2-dy)<roundoff_tol) && (abs(dx2-dx)<roundoff_tol)){
            # FIXME: roundoff_tol here should be different to the one used previously.
            # Might matter in unusual cases
            output=us_dist[k+1] + ds2
            connecting_segment=k
            break
        }
    }
    

    if(is.na(output) | is.na(connecting_segment)){
        #print('output / connecting_segment is NA in usdistfun ( useful_functions.R). Going into browser()')
        #browser()
        #print('XXX')
        #print('XXX')
        #print('XXX')
        print('output / connecting_segment is NA in usdistfun ( useful_functions.R). ')
        stop()
    }
    return(c(output, connecting_segment))
}


####################################################################################

#' Check that storage areas are connected to something
#'
#' In hec-ras, weird things can happen if you have a storage area with no hydraulic connections
#' This function checks that all storage areas are connected to something
#'
#' @param hecras_file filename of hecras geometry
#' @return A print statement if all storage areas are connected. Otherwise,
#'         browser() is called so you can check what happens
#' @export

check_storage_connectedness<-function(hecras_file){
    # Utility function to check that all storage areas in a .g0x file (hecras
    # geometry file) are connected

    # Read file
    hec_lines=readLines(file(hecras_file))

    ## Find storage areas

    # Indices of storage areas
    SA_lines=grep('Storage Area=', hec_lines)
    # Names of storage areas
    SA_names=substring(hec_lines[SA_lines], 14, 14+16 -1)

    ## Find Connection with storage connections and lateral weirs

    # Storage connections
    conn_up_lines=grep('Connection Up', hec_lines)
    conn_up_names=substring(hec_lines[conn_up_lines], 18, 18+16-1)
    conn_dn_lines=grep('Connection Dn', hec_lines)
    conn_dn_names=substring(hec_lines[conn_dn_lines], 18, 18+16-1)

    # Lateral weirs
    conn_weir_lines=grep('Lateral Weir End=', hec_lines)
    conn_weir_names=substring(hec_lines[conn_weir_lines], 61, 61+18-1)

    all_connected_names=union(conn_up_names,conn_dn_names)
    all_connected_names=union(all_connected_names, conn_weir_names)


    if(length(all_connected_names)!=length(SA_names)){
        print('Not all storage areas have connections: Take a look')
        browser() 
    }else{
        print('All storage areas have connections')
    }

    return()
}
