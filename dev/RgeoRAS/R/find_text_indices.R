# CODES TO FIND PATTERNS IN HECRAS FILES
# OFTEN WE DON'T NEED TO EXPORT THESE
#
#

#
# Find start/end indices of channel cutlines
#
#param hec_lines Character vector containing hecras geometry
#return 2 column matrix with start/end indices of cutlines
#examples
# hec_lines=readLines(paste0(system.file(package='RgeoRAS'), '/extdata/Geo_with_xsect.g02'))
# cutline_indices=get_cutline_index_bounds(hec_lines)
get_cutline_index_bounds<-function(hec_lines){
    ll = length(hec_lines)
    # Identify the GIS cutlines in the hec-ras file
    GIS_cutline_start=grep('XS GIS', hec_lines) # Line before the GIS Cutline data
    GIS_cutline_end=GIS_cutline_start*NA # 
    for(i in 1:length(GIS_cutline_start)){
        # Line after cutline = the first occurrence of the 'Node Last Edited' text
        upper_search=min(ceiling(glob_vars$max_cutline_pts/2), ll-GIS_cutline_start[i])
        tmp=grep('Node Last', hec_lines[GIS_cutline_start[i] + 1:upper_search])[1]
        if(length(tmp)==0) stop('Failed to find end of cutline -- could be due to having too small a value of max_cutline_pts (?)')
        GIS_cutline_end[i]=tmp + GIS_cutline_start[i]
    }
    GIS_cutline_bounds=cbind(GIS_cutline_start+1, GIS_cutline_end-1)

    return(GIS_cutline_bounds)
}

# Find start/end indices of xsections
#
get_xsection_xy_index_bounds<-function(hec_lines){
    print('EXTRACTING XSECTION XY BOUNDS FROM HECRAS FILE')
    # Identify the xsectional profile data in the hec-ras file
    xsect_1=grep('#Sta', hec_lines) +1 # Start of cross-sectional (x,z) data
    xsect_2=grep('#Mann', hec_lines) -1 # End of cross-sectional (x,z) data
    xsect_bounds=matrix(c(xsect_1,xsect_2),ncol=2) # These pairs can be used to get the cross-sectional data
    return(xsect_bounds)
}


get_mann_index_bounds<-function(hec_lines){
    # Identify the manning's n data
    mann_start=grep('#Mann=', hec_lines)
    mann_end=grep('Bank Sta=', hec_lines) - 1
    # NOTE: Sometimes 'levee' layers occur just before "Bank Sta="
    # So we need to correct for this
    for(i in 1:length(mann_end)){
        checklev=grep('Levee', hec_lines[mann_end[i]])
        if(length(checklev)>0){
            mann_end[i] = mann_end[i]-1
        }
    }
    return(cbind(mann_start, mann_end))
}
