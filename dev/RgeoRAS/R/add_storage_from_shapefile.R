require(rgdal)
require(raster)
require(rgeos)

############################################################

#' Add storage areas to the hecras geometry with a shapefile
#'
#' 
#' @param hec_geo Filename of the hecras geometry, or a character vector produced from such a file with readLines
#' @param new_storage Filename of a polygon shapefile containing the new storage areas, or a SpatialPolygonsDataFrame with the new storage areas
#' @param dem Filename of a DEM to use to get the elevations in the storage areas, or a RasterLayer with the DEM
#' @param dem_vertical_offset = Number added to all values extracted from the DEM (to deal with datum offsets
#' @param upper_bound_storage_offset Offset to upper bound on stage-volume curves: see \code{upper_bound_stage_offset} in \code{compute_storage_stage_vol_relation}
#' @param limit_weir_elevation_by_channel_bank_elevation TRUE/FALSE Should the lateral weir elevation be forced to be > the channel bank elevation
#' @param lower_limit_on_lateral_weir_elevations numeric value which all lateral weir elevations are clipped to be greater than or equal to
#' @param output_file If not NULL, write the new hec-lines to a file with this name
#' @param verbose TRUE/FALSE -- verbosely print progress messages
#' @return If \code{output_file=NULL}, then return a character vector with the new hecras geometry. Otherwise, write this character vector to a file, and return nothing.
#' @export
#' @details
#'      Storage areas are connected to each other if they touch when slightly buffered. See code for details. 
#'
#'      Storage areas are connected to the channel if they contain 2 or more bank points. If they touch the channel polygon without containing enough bank points, a message is printed.
#'
#'      The hydraulic parameters for the storage area connections are randomly chosen, expect to have to edit these.
#' 
#' @examples
#'     mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/Geo_with_xsect.g02')
#'     mydem=paste0(system.file(package='RgeoRAS'), '/extdata/SRTM_clipped_PRS92Z3_comp.tif')
#'     myStoragePoly=paste0(system.file(package='RgeoRAS'), '/extdata/Storage/Store1.shp')
#'     new_ras_file=add_storage_from_shapefile(mychanfile, myStoragePoly, mydem)
# 
add_storage_from_shapefile<-function(hec_geo, new_storage, dem, dem_vertical_offset=0, 
                               limit_weir_elevation_by_channel_bank_elevation=FALSE,
                               lower_limit_on_lateral_weir_elevations=-Inf,
                               upper_bound_storage_offset=100.,
                               output_file=NULL, verbose=TRUE){

    #@ Magic numbers for buffering / removing invalid geometries
    chanBufferWidth1=glob_vars$chanBufferWidth1
    newStoreBufferWidth1=glob_vars$newStoreBufferWidth1
    storage_area_overlap_merge_threshold=glob_vars$storage_area_overlap_merge_threshold
    storage_conn_buffer=glob_vars$storage_conn_buffer

    #@ Get the geometry data
    if(length(hec_geo)==1 && file.exists(hec_geo)){
        if(verbose) print('Reading geometry ...')
        hec_lines=readLines(hec_geo)
    }else{
        hec_lines=hec_geo
    }

    #@ Get the new storage areas
    if(class(new_storage)=='character' && file.exists(new_storage)){
        if(extension(new_storage)!='.shp') stop('new_storage must be a shapefile, with name ending in .shp')
        if(verbose) print('Reading storage shapefile ...')
        new_storage_dsn=dirname(new_storage)
        new_storage_layer=gsub(".shp", "", basename(new_storage))
        new_storage=readOGR(dsn=new_storage_dsn,layer=new_storage_layer)
    }
    #@ Buffer to make geometry valid
    new_store_simp=gBuffer(new_storage,width=newStoreBufferWidth1,byid=T)

    #@ Get the DEM
    if(class(dem)=='character' && file.exists(dem)){
        if(verbose) print('Reading dem ...')
        dem=raster(dem)
    }

    #@ Get geometry as Spatial* objects
    spatial_proj=dem@crs

    if(verbose) print('Extracting spatial data from hecras geometry ...')
    chan=spatial_channel_polygon(hec_lines, spatial_proj)
    chan2=gBuffer(chan,width=chanBufferWidth1,byid=T) # Buffer to make geometry 'valid'
    
    chan_boundary_points=spatial_channel_boundary_points(hec_lines,spatial_proj)

    chan_cutlines=spatial_channel_cutlines(hec_lines,spatial_proj)

    #chan_junctions=spatial_junction_polygon(hec_lines, chan_cutlines)

    #centrelines=spatial_centrelines(hec_lines,spatial_proj)

    old_storage=spatial_storage_areas(hec_lines,spatial_proj)

    #@ Get bridge lines
    bridge_lines=grep('Bridge Culvert', hec_lines) 

    #@ Convert new_store_simp to list of SpatialPolygons with minimal intersection
    new_store_list=list()
    for(i in 1:length(new_store_simp)){
        new_store_list[[i]]=SpatialPolygons(new_store_simp@polygons[i], proj4string=spatial_proj)
    }

    if(length(new_store_list)>1){
        # Remove intersections that are too large from new_store_list, by merging polygons
        new_store_list_less_intersect=list()
        counter=0
        for(i in 1:(length(new_store_list)-1)){
            APPEND=TRUE
            for(j in (i+1):length(new_store_list)){

                if(gIntersects(new_store_list[[i]], new_store_list[[j]])){
                    # Check how large the intersection is compared with each polygon
                    ij_intersect=gIntersection(new_store_list[[i]], new_store_list[[j]])

                    intersection_threshold=storage_area_overlap_merge_threshold*
                                           min(gArea(new_store_list[[i]]), gArea(new_store_list[[j]]))
                    if(gArea(ij_intersect) > intersection_threshold){
                        # Intersection is too large -- add polygon i to j, and move on
                        #browser()
                        warning(paste('Removing overly large intersection from the new storage area files', i, j),
                                immediate=TRUE)
                        new_store_list[[j]]=gUnion(new_store_list[[i]], new_store_list[[j]])
                        APPEND=FALSE
                    }
                }
                
            }
            # If there is no overly large intersection, keep
            if(APPEND==TRUE){
                counter=counter+1
                new_store_list_less_intersect[[counter]]=new_store_list[[i]]
            }
        }
        # Add in the last polygon in new_store_list    
        counter=counter+1
        new_store_list_less_intersect[[counter]] = new_store_list[[length(new_store_list)]]

        # Update new_store_list
        new_store_list=new_store_list_less_intersect
    }
    # Buffer to make valid
    for(i in 1:length(new_store_list)){
        new_store_list[[i]] = gBuffer(new_store_list[[i]], width=newStoreBufferWidth1)
    }
   
    #@ Convert old_storage to a list of spatialPolygons 
    if(!is.null(old_storage)){
        old_store_list=list()
        for(i in 1:length(old_storage@polygons)){
            old_store_list[[i]]=SpatialPolygons(old_storage@polygons[i], proj4string=spatial_proj)
        }
    }

    #@ Convert chan2 to a list of spatialpolygons
    chan2_list=list()
    for(i in 1:length(chan2)){
        chan2_list[[i]]=SpatialPolygons(chan2@polygons[i], proj4string=spatial_proj)
    }
    

    #@ Step 1.: Identify intersections of storage polygons with each other, or with the channel network
    #@           While we do this, we make a set of polygons including the unique parts of storage areas
    if(verbose) print("COMPUTING 'NEW STORAGE'-'NEW STORAGE' AREA INTERSECTIONS")
    new_storage_intersections=list()
    unique_new_store_list=list() # This will store the unique part of the storage area
    for(i in 1:length(new_store_list)){
        #unique_new_store_list[[i]]=gSimplify(new_store_list[[i]], tol=0.1, topologyPreserve=TRUE) # Trick to try to force valid geometries
        unique_new_store_list[[i]]=new_store_list[[i]]

        intersections=c()
        for(j in i:length(new_store_list)){
            # Note that we only loop from i, so we avoid double counting connections
            if(i==j) next

            if(gIntersects(unique_new_store_list[[i]], new_store_list[[j]])){
                intersections=c(intersections,j)
                if(FALSE){
                    # Record the non-intersecting area -- can be prone to problems
                    unique_new_store_list[[i]] = gDifference(unique_new_store_list[[i]], new_store_list[[j]])
                    unique_new_store_list[[i]] = gBuffer(unique_new_store_list[[i]], width=0.) # Hack to make valid
                }
            } 
        }
        if(is.null(intersections)){
            new_storage_intersections[[i]]=NA
        }else{
            new_storage_intersections[[i]]=intersections
        }
    } 

    # Compute new-storage / old storage intersections
    if(!is.null(old_storage)){ 
        if(verbose) print("COMPUTING 'NEW STORAGE' - 'OLD STORAGE' AREA INTERSECTIONS")
        old_storage_intersections=list()
        if(length(old_store_list)>0){
            for(i in 1:length(new_store_list)){
                intersections=c()
                for(j in 1:length(old_store_list)){
                    
                    if(gIntersects(new_store_list[[i]], old_store_list[[j]])){
                        intersections=c(intersections,j)
                    } 
                }
                if(is.null(intersections)){
                    old_storage_intersections[[i]]=NA
                }else{
                    old_storage_intersections[[i]]=intersections
                }
            }
        }
    }

    # Compute new-storage / channel intersections
    if(verbose) print("COMPUTING 'NEW STORAGE' - CHANNEL AREA INTERSECTIONS")
    channel_intersections=list()
    for(i in 1:length(new_store_list)){
        intersections=c()
        for(j in 1:length(chan2_list)){
            if(gIntersects(new_store_list[[i]], chan2_list[[j]])){
                intersections=c(intersections,j)
            } 
        }
        if(is.null(intersections)){
            channel_intersections[[i]]=NA
        }else{
            channel_intersections[[i]]=intersections
        }
    } 


    # Compute stage volume relations for storage areas in new_store_list
    if(verbose) print("COMPUTING STAGE-VOLUME RELATIONS FOR STORAGE AREAS")
    new_store_stage_vol_list=list()
    for(i in 1:length(new_store_list)){
        if(verbose) cat(paste(i,'..'))
        my_poly=new_store_list[[i]]
        new_store_stage_vol_list[[i]] = compute_storage_stage_vol_relation(my_poly,dem, dem_vertical_offset, upper_bound_storage_offset)
    }

    # Construct new storage area geometries
    #@
    #@ Step 2.1 - Make text describing the storage areas, that can be inserted into hecras .g01 file
    #@
    if(verbose) print("CONSTRUCTING 'NEW STORAGE AREA' GEOMETRIES")
    storage_text=list()
    new_storage_names=list()
    #@ We wish to keep the names of storage areas unique. One way to attempt to
    #@ do this is to append a timestamp to the name
    name_timestamp=floor(as.numeric(Sys.time())*100)%%10000000+i # Generic stamp for storage name
    for(i in 1:length(new_store_list)){
        #name=paste('Fake_store',i,sep="") # Name for storage area
        new_storage_names[[i]]=paste('S',(name_timestamp+i),sep="") # Name for storage area
        storage_text[[i]]=make_storage_area_text(new_store_list[[i]], new_store_stage_vol_list[[i]], new_storage_names[[i]])
    }

    #@ Append to a new hecras file = hec_lines2
    hec_lines2=hec_lines
    end_storage=grep('Connection=', hec_lines2)[1]-2 # We need to insert storage areas here
    if(is.na(end_storage)){
        end_storage=grep('Chan Stop Cuts', hec_lines2)[1] -2 
    }

    hec_tmp=hec_lines2[1:end_storage]
    #@ Append storage
    for(i in 1:length(storage_text)){
        hec_tmp=c(hec_tmp, " ") # Add a newline
        hec_tmp=c(hec_tmp,storage_text[[i]]) # Add the storage area
    }
    #@ Append the rest of geometry file
    hec_tmp=c(hec_tmp, hec_lines2[(end_storage+1):length(hec_lines2)])
    #@ Update the new hecras file
    hec_lines2=hec_tmp

    #@ Construct storage area connections at new/new intersections
    #@
    #@ Step 2.2 -- Loop over all overlapping storage areas, and make a storage area connection
    #@
    if(verbose) print("CONSTRUCTING STORAGE AREA CONNECTION GEOMETRIES AT 'NEW STORAGE'-'NEW STORAGE' INTERSECTIONS")
    storage_connection_text_all=c()
    for(i in 1:length(new_storage_intersections)){
      intersections=new_storage_intersections[[i]]
      if(is.na(intersections[1])) next
      
      for(j in 1:length(intersections)){
          k=intersections[j]
          storage_connection_text=
              make_storage_connection_text( new_store_list[[i]], new_store_list[[k]], 
                                            new_storage_names[[i]], new_storage_names[[k]],
                                            dem, vertical_datum_offset = dem_vertical_offset,
                                            storage_area_overlap_merge_threshold = storage_area_overlap_merge_threshold)
          if(!is.na(storage_connection_text[1])){
              storage_connection_text_all=c(storage_connection_text_all, storage_connection_text, " ")
          }
      }


    }

    #@
    #@ Insert storage area connection text into the output file
    #@

    #@ find appropriate index
    suppressWarnings({end_storage_inds=max(grep("^Conn HTab HWMax=", hec_lines2))}) # Last or second last line of storage areas
    if(!is.finite(end_storage_inds)){
        end_storage_inds=grep('Chan Stop Cuts', hec_lines2)[1] -2 
    }

    if(hec_lines2[end_storage_inds+1]!=""){
        end_storage_inds=end_storage_inds+2
    }else{
        end_storage_inds=end_storage_inds+1

    }

    #@ Do insertion
    ll=length(hec_lines2)
    hec_linestmp=c(hec_lines2[1:end_storage_inds], 
                   storage_connection_text_all, 
                   hec_lines2[(end_storage_inds+1):ll])

    hec_lines2=hec_linestmp

    #@ Construct storage area connections at new/old intersections
    #@
    #@ Step 2.3 -- Loop over all overlapping storage areas, and make a storage area connection
    #@
    if(!is.null(old_storage)){
        if(verbose) print("CONSTRUCTING STORAGE AREA CONNECTION GEOMETRIES AT 'NEW STORAGE'-'OLD STORAGE' INTERSECTIONS")
        storage_connection_text_all=c()
        for(i in 1:length(old_storage_intersections)){
          intersections=old_storage_intersections[[i]]
          if(is.na(intersections[1])) next
          
          for(j in 1:length(intersections)){
              k=intersections[j]
              storage_connection_text=
                  make_storage_connection_text( new_store_list[[i]], old_store_list[[k]], 
                                                new_storage_names[[i]], old_storage$name[k],
                                                dem,dem_vertical_offset)    
              if(!is.na(storage_connection_text[1])){
                  storage_connection_text_all=c(storage_connection_text_all, storage_connection_text, " ")
              }
          }
        }
    }

    #@
    #@ Insert storage area connection text into the output file
    #@

    #@ find appropriate index
    suppressWarnings({end_storage_inds=max(grep("^Conn HTab HWMax=", hec_lines2))}) # Last or second last line of storage areas
    if(!is.finite(end_storage_inds)){
        end_storage_inds=grep('Chan Stop Cuts', hec_lines2)[1] -2 
    }
    if(hec_lines2[end_storage_inds+1]!=""){
        end_storage_inds=end_storage_inds+2
    }else{
        end_storage_inds=end_storage_inds+1

    }

    #@ Do insertion
    ll=length(hec_lines2)
    hec_linestmp=c(hec_lines2[1:end_storage_inds], 
                   storage_connection_text_all, 
                   hec_lines2[(end_storage_inds+1):ll])

    hec_lines2=hec_linestmp

    # Add lateral structures at new-storage/channel interface
    #@
    #@
    #@ Make a spatial points object holding the channel cross-section end points, and their section label
    #@ We can use this to associate lateral structures with x-sections 
    #@
    #@
    #stop()
    # Loop over all storage-channel intersections
    if(verbose) print("CREATING LATERAL STRUCTURES AT 'NEW STORAGE'-CHANNEL INTERSECTIONS")
    hec_linestmp=hec_lines2 # Copy output file for modification
    for(i in 1:length(channel_intersections)){

        if(is.na(channel_intersections[[i]][1])) next
            
        #@ First:
        unique_new_store_list[[i]]=gBuffer(unique_new_store_list[[i]],width=2.) # Buffer helps to avoid skipping the odd point

        for(j in 1:length(channel_intersections[[i]])){
            k=channel_intersections[[i]][j]
            print(c(i,k))
            hec_linestmp_old=hec_linestmp

            #@ Iteratively update hec_linestmp by inserting lateral weir
            #@ Use only unique parts of storage areas -- HEC RAS cannot have
            #@ overlapping lateral structures. Also, don't let lateral
            #@ structures span over bridges

            #@ TRICKS TO MAKE THINGS WELL BEHAVED.
            #@ Make sure that the chan_boundary_points we search through actually belong to this channel
            #@ , as sometimes we can get points intersecting multiple channels
            #@ 
            kk=which(!is.na(over(chan_boundary_points, chan2_list[[k]]))) # Indices of bank points within channel poly
            reachlist=sort( table( as.character(chan_boundary_points@data[kk,1])),decreasing=TRUE) # Names of reaches
            reachname=names(reachlist)[1] # Most commonly occurring name -- this will be the reach name
            chan_tmp_points=which(as.character(chan_boundary_points@data[kk,1])==reachname)
            chan_tmp_points=chan_boundary_points[kk[chan_tmp_points],]

            #@ We cannot have a lateral structure between the first and second points in a reach.
            #@ Also, we tend to have problems if we put it near the most downstream point.
            #@ So let's remove the first points from chan_tmp_points.
            remove=c(1, min(which(chan_tmp_points$bank=='R')))
            chan_tmp_points=chan_tmp_points[-remove,]
            remove=c( max(which(chan_tmp_points$bank=='L')), length(chan_tmp_points[,1]))
            chan_tmp_points=chan_tmp_points[-remove,]

            #@ We must ensure that the elevations along the lateral structure
            #@ are not below the min elevation in the storage area
            min_structure_elev=min(new_store_stage_vol_list[[i]][,1])
            if(verbose) print(c('#3322311# ', min_structure_elev))


            #@ 
            hec_linestmp=make_lateral_weir_text(unique_new_store_list[[i]], new_storage_names[[i]],
                                                     chan2_list[[k]], chan_tmp_points, 
                                                     dem, dem_vertical_offset, 
                                                     hec_linestmp,
                                                     bridge_lines, 
                                                     limit_weir_elevation_by_channel_bank_elevation,
                                                     min_structure_elev, lower_limit_on_lateral_weir_elevations)

            # Trick to join up storage areas which are 'just' missing the channel point.
            l1=length(hec_linestmp_old)
            l2=length(hec_linestmp)
            l3=min(l1,l2)
            if(all(hec_linestmp_old[l3]==hec_linestmp[l3])& (l2==l1)){
                #browser()
                unique_new_store_list[[i]]=gBuffer(unique_new_store_list[[i]],width=storage_conn_buffer) 
                hec_linestmp=make_lateral_weir_text(unique_new_store_list[[i]], new_storage_names[[i]],
                                                         chan2_list[[k]], chan_tmp_points, 
                                                         dem, dem_vertical_offset, 
                                                         hec_linestmp,
                                                         bridge_lines, 
                                                         limit_weir_elevation_by_channel_bank_elevation,
                                                         min_structure_elev, lower_limit_on_lateral_weir_elevations)


            }

        }

    }

    hec_lines2=hec_linestmp

    #@ Write to output
    if(!is.null(output_file)){
        heclines_2_File(hec_lines2, output_file)
        return()
    }else{
        return(hec_lines2)
    }

}


