require(rgdal)
require(raster)
require(rgeos)

##########################################################################################

#' Compute the 'downstream distances' of the xsections on each
#' reach, using the cutline and reach centreline information in hec_lines
#'
#' Assumes that left bank distance = right bank distance = straight line 
#' distance between x-sections at intersection with the channel,
#' while channel distance = distance along the channel.
#'
#' NOTE: We are presently setting the left bank and right bank distances to be the same.
#' This should be appropriate if the overbank flows are largely occurring near the channel
#' However, this will not always be correct, although it will often be true
#' if the floodplain roughness gets high away from the channel. 
#' In reality, the left bank / right bank distances should depend on the
#' flow state -- during serious inundation, they might differ quite a bit. 
#'
#' @param hec_lines vector of character strings from the hecras file (which may have been modified already)
#' @param chan_cutlines channel cutlines SpatialLinesDataFrame -- output from spatial_channel_cutlines
#' @param reach_lines reach centrelines SpatialLinesDataFrame -- output from spatial_centrelines
#' @return modified version of hec_lines (with updated downstream distances)
#' @export
#' @examples
#'   #Nice_example
#'   x=2
#'   \dontrun{
#'       print('FIXME: The following fails')
#'       require(rgdal)
#'       mychanfile=paste0(system.file(package='RgeoRAS'), '/extdata/marikina.g36')
#'       hec_lines=readLines(mychanfile)
#'       my_reach_lines=spatial_centrelines(mychanfile, CRS("+init=epsg:3123"))
#'       my_cutlines=spatial_channel_cutlines(mychanfile,CRS("+init=epsg:3123")) 
#'       usdists=update_downstream_distances(hec_lines, my_cutlines, my_reach_lines)
#'   }
#'
update_downstream_distances<-function(hec_lines, chan_cutlines, reach_lines){
    #@ Function to modify the 'downstream distances' of the xsections on each
    #@ reach in hec_lines, using the cutline and reach centreline information in hec_lines.
    
    #@ Assume that left bank = right bank = straight line 
    #@ distance between x-sections at intersection with the channel,
    #@ while channel distance = distance along the channel.

    # NOTE: We are setting the left bank and right bank distances to be the same.
    # This should be appropriate if the overbank flows are largely occurring near the channel
    # However, this will not always be correct, although it will often be true
    # if the floodplain roughness gets high away from the channel. 
    # In reality, the left bank / right bank distances should depend on the
    # flow state -- during serious inundation, they might differ quite a bit. 
    
    spatial_proj=proj4string(chan_cutlines)

    # Now, for every reach_line, find its intersection with the centreline.
    # This should be just one point: if more than that, we need to do some more
    # work
    #downstream_distances_straight=list()
    #downstream_distances_chan=list()

    for(i in 1:length(reach_lines)){
        # Get cross-sections located on this reach
        local_xsects_inds=which(chan_cutlines@data$reach==reach_lines@data$reach[i])
        local_xsects = SpatialLines(chan_cutlines@lines[local_xsects_inds], proj4string=CRS(spatial_proj))
        local_xsects_df = SpatialLinesDataFrame(local_xsects, data=chan_cutlines@data[local_xsects_inds,])#, proj4string=CRS(spatial_proj))
      
        # Get the reach line as a SpatialLines 
        local_centreline=SpatialLines(reach_lines@lines[i], proj4string=CRS(spatial_proj))

        #cutpoints=gIntersection(local_centreline,local_xsects,byid=T)

        # Loop over the xsections, and find their upstream_distance with the
        # channel, and intersection_point
        intersect_pt=c()
        upstream_distances_chan=rep(NA, length(local_xsects))
        for(j in 1:length(local_xsects)){
            cutpointz=gIntersection(local_xsects[j], local_centreline)

            # Treat the case of multiple intersections by choosing the one that
            # is most upstream
            if(length(cutpointz)>1){
                # Compute upstream distances of points along channel
                usdists=rep(NA,length(cutpointz))            
                for(k in 1:length(usdists)){
                    tmp = usdistfun(coordinates(cutpointz[k]), coordinates(local_centreline)[[1]][[1]])
                    usdists[k]=tmp[1]
                }
                # Select the point with the largest upstream distance
                keep_ind=which.max(usdists)
                cutpointz=cutpointz[keep_ind]
                upstream_distances_chan[j]= usdists[keep_ind]
                
            }else if(length(cutpointz)==0){
                print('no cutpoint')
                browser()
                stop('no cutpoint')
            }else{
                tmp = usdistfun(coordinates(cutpointz), coordinates(local_centreline)[[1]][[1]])
                upstream_distances_chan[j]=tmp[1] 
            }
            intersect_pt=rbind(intersect_pt, coordinates(cutpointz))
        }
        
        downstream_distances_chan=c(-diff(upstream_distances_chan), 0.)
        downstream_distances_straight=c((diff(intersect_pt[,1])**2 + diff(intersect_pt[,2])**2)**0.5, 0)
      
        # Now correct the hec_lines file 
        reach_index=grep(as.character(reach_lines@data$reach[i]), hec_lines)
        l = length(hec_lines)
        for(j in 1:length(local_xsects)){
            old_xsect_txt=as.character(local_xsects_df@data[j,]$sec_info) # Text string corresponding to the cross_section
            index_in_hecfile=grep(old_xsect_txt,hec_lines[reach_index:l], fixed=TRUE)[1] + reach_index-1 # Ensure that it occurs within the reach xsections

            text_split=strsplit(old_xsect_txt,",")[[1]]

            # Change the downstream distances
            if(downstream_distances_chan[j]!=0.){
                text_split[c(3,5)]=as.character(round(downstream_distances_straight[j],2))
                chan_dist=max(downstream_distances_straight[j], downstream_distances_chan[j])
                text_split[4] = as.character(round(chan_dist,2))
                

                text_combine=paste(text_split,collapse=",")
                hec_lines[index_in_hecfile]=text_combine
            }
                        
        }
    }

    return(hec_lines)
}

