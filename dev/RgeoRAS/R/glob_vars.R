# Aim to gradually migrate 'magic numbers' from code into glob_vars list, so we can keep track of what they do

glob_vars=list(


                #################################################
                #
                # STORAGE AREA FROM SHAPEFILE PARAMETERS
                #
                #################################################

                # We simplify storage area polygons using gSimplify, with this tolerance (units m)
                storage_poly_simplify_tol=10.,

                # If there are > this many points in a storage area AFTER
                # simplification, then forcefully reduce the number of points
                # with re-interpolation
                thin_storage_poly_pointcount=50,

                # Maximum number of points allowed in storage area polygon
                max_storage_poly_pointcount=499,

                # It is bad to have storage areas which significantly overlap, since it leads to double counting of mass.
                # If the fraction of storage area overlap is > (storage_area_overlap_merge_threshold) x (the polygon area)
                # , we can merge the overlappying storage areas (with a warning). 
                # If we try to make a connection between 2 storage areas with an overlap fraction which is greater than this,
                # the code will throw an error
                storage_area_overlap_merge_threshold=0.2,

                # When adding storage areas from shapefile, buffer channel by this distance (m)
                # If overlap is detected between (buffered) channel and
                # (buffered) storage areas, they may be connected.
                chanBufferWidth1=1.0e-03 ,

                # When adding storage areas from shapefile, buffer them by this distance (m)
                # Storage areas which then overlap will be hydraulically connected 
                newStoreBufferWidth1=1.0e-03 ,

                # When new-storage areas are 'just' missing a channel point,
                # buffer them by this distance, and if they connect after than,
                # then make a connection
                storage_conn_buffer=20. ,

   
                ##################################################
                #
                # STORAGE AREA CONNECTIONS [Between 2 storage areas]
                #
                ##################################################
                
                # Weir hydraulic defaults 
                store_conn_weir_properties=
                            c("Conn Routing Type= 1",  
                             "Conn Use RC Family=True",
                             "Conn Weir WD=20",
                             "Conn Weir Coef=1.66",
                             "Conn Weir Is Ogee= 0", 
                             "Conn Simple Spill Pos Coef=0.05",
                             "Conn Simple Spill Neg Coef=0.05"),

                # Weir length = (perimeter of polygon defining the intersection of the storage areas)*weir_length_scaling
                # For example, for a long, thin connection which was entirely weir, weir_length_scaling ~=1/2, since
                # the length of the connection would be ~ half the perimeter of the intersection polygon
                weir_length_scaling=1/3,

                # Minimum weir length (m). If weir length would be less than this, do not create a storage area connection
                min_weir_length=5.0,
                
                # Maximum number of points defining the elevation of the weir
                max_weir_points=30,

                # Htab parameter hwmax will be max_weir_elevation +
                # weir_htab_hwmax_offset (m). Water elevation should never exceed
                # this
                weir_htab_hwmax_offset=15,

                ######################################################
                #
                # Channel-to-storage connections
                #
                ######################################################

                # Maximum number of bank points which can be included in a
                # single lateral weir, connecting a storage area with the
                # channel
                max_latweir_chanpts=99,

                # Maximum number of points defining the elevation of a channel-to-storage weir
                max_latweir_chan2storage_pointcount=80,

                # Distance downstream (m) from the most upstream cross-section, where
                # the lateral weir begins
                lat_weir_downstream_distance=1.0,

                # When building a weir, trim it by this much so avoid overshoots downstream.
                weir_line_trim=3.0,

                # Default hydraulic properties of channel-2-storage lateral weirs
                lat_weir_properties=
                c("Lateral Weir WD=20",
                  "Lateral Weir Coef=1.1",
                 "Lateral Weir WSCriteria=-1",
                 "Lateral Weir Flap Gates= 0",
                 "Lateral Weir Hagers EQN= 0 ,,,,,",
                 "Lateral Weir SS=0.05,0.05,",
                 "Lateral Weir Type= 0",
                 "Lateral Weir Connection Pos and Dist= 0 ,"),


                ########################################################
                #
                # Channel cross-section defaults
                #
                ########################################################

                # Expansion/contraction coefficients as text
                expansion_and_contraction='Exp/Cntr=0.3,0.1',

                # Htab curves will have their lowest point = min(bed_elevation)+htab_start_above_bed (m)
                htab_start_above_bed_min=0.01,
           
                # We will have this many points on the cross-section 
                htab_number_of_points=100,

                # Number of lines we look ahead, to find the next river station
                next_station_linesearch=300,

                # Acceptable difference between the length of a cross-section
                # cutline, and the length of its associate distance-elevation profile (units m)
                # Some routines check that the lengths are equal to within this
                # tolerence, and will throw an error if not
                section_alignment_tol=1.0e-0,

                # Used to search through cutline text efficiently -- a cutline
                # should have < this number of points
                max_cutline_pts=200

)
