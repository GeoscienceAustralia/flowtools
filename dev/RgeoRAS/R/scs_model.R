### R implementation of the scs runoff volume / unit hydrograph model from
### hec-hms.
#
#
# Key functions:
#
# SCS_runoff_volume: Compute runoff volumes using the SCS method
# SCS_uh: route the runoff volumes through an SCS unit hydrograph

##################################################
#
# TESTS
#
##################################################

#' Test the SCS runoff model by comparison with hec-hms outputs
#' @return Some plots + a pass/fail statement
#' @export
test_SCS_runoff<-function(){
    # Check that the SCS_runoff_volume function gives the same results as hec-hms

    # #Get some data to experiment
    bosodata=paste0(system.file(package='RgeoRAS'), '/extdata/bosRAIN.csv')
    boso_R=read.csv(bosodata, header=F, 
                colClasses=c('character', 'numeric', 'numeric', 'numeric', 'character', 'numeric'), 
                na.strings=c('no_data', ' ', '*', '*.*'),
                col.names=c('station', 'year', 'month', 'day', 'hour', 'rain (mm)') )

    # Get indices for 24Sep 00:00 through to 30 Sept 00:00 -- note the weird time notation in the data
    v=which(boso_R[,2]==2009 & boso_R[,3]==9 & boso_R[,4]%in%c(23,24, 25,26,27, 28, 29))
    v=v[-(1:16)]
    v=v[ - ((length(v)-7):length(v))]

    # Compute runoff volumes using the above script
    rv=SCS_runoff_volume(3600, boso_R[v,6], 74, Area=1.0, dt=60.)

    # Convert to the same units as hec-hms [ mm ], and adjust for the time notation
    x1=rv[,2]/1000.

    # Get the same thing, computed with hec-hms.
    hmsdata=paste0(system.file(package='RgeoRAS'), '/extdata/HMS_output.csv')
    hms_output=read.csv(hmsdata, header=T, 
                        colClasses=c(rep('character',2), rep('numeric', 6)))

    x2=hms_output[,5]
    #print(length(x1), length(x2))
    par(mfrow=c(2,1))
    #plot(x1-x2, main='This function less hec-hms')
    plot(x1,t='l', main='R & HMS overplotted')
    points(x2,t='l',col=2)

    plot(round(x1,2)-x2, main='The difference (rounded) ')

    if(max(abs(round(x1,2)-x2),na.rm=T)==0.){
        print('PASS: No difference when rounded to 2 decimal places')
    }else{
        print('FAIL: Error is beyond round-off error')
    }

}

################################################################

#' Test the SCS unit hydrograph function by comparison with hec-hms
#'
#' @return Produce some comparison plots
#' @export
test_SCS_uh<-function(){
    # Check that the SCS unit hydrograph function gives the same results as hec-hms
    wawa_CN=74
    wawa_t_lag=168*60
    wawa_Area=287
    wawa_dt=60

    bosodata=paste0(system.file(package='RgeoRAS'), '/extdata/bosRAIN.csv')
    boso_R=read.csv(bosodata, header=F, 
                colClasses=c('character', 'numeric', 'numeric', 'numeric', 'character', 'numeric'), 
                na.strings=c('no_data', ' ', '*', '*.*'),
                col.names=c('station', 'year', 'month', 'day', 'hour', 'rain (mm)') )
    # #Get some data to experiment
    #boso_R=read.csv('./data/bosRAIN.csv', header=F, 
    #            colClasses=c('character', 'numeric', 'numeric', 'numeric', 'character', 'numeric'), 
    #            na.strings=c('no_data', ' ', '*', '*.*'),
    #            col.names=c('station', 'year', 'month', 'day', 'hour', 'rain (mm)') )

    # Get indices for 24Sep 00:00 through to 30 Sept 00:00 -- note the weird time notation in the data
    v=which(boso_R[,2]==2009 & boso_R[,3]==9 & boso_R[,4]%in%c(23,24, 25,26,27, 28, 29))
    v=v[-(1:16)]
    v=v[ - ((length(v)-7):length(v))]

    # Compute runoff volumes using the above script
    rv=SCS_runoff_volume(3600, boso_R[v,6], wawa_CN, Area=wawa_Area, dt=wawa_dt)

    hydrograph=SCS_uh(rv, wawa_t_lag)
    x1=hydrograph[,2]
    # Get the same thing, computed with hec-hms.
    hmsdata=paste0(system.file(package='RgeoRAS'), '/extdata/HMS_output.csv')
    hms_output=read.csv(hmsdata, header=T, 
                        colClasses=c(rep('character',2), rep('numeric', 6)))
    #hms_output=read.csv('./data/HMS_output.csv', header=T, 
    #                    colClasses=c(rep('character',2), rep('numeric', 6)))

    x2=hms_output[,8]
    #print(length(x1), length(x2))
    par(mfrow=c(2,1))
    plot(x1, main='Both hydrographs')
    points(x2,t='l', col=3)

    plot(x1-x2, main='Difference in the hydrographs (very small -- interpolation related??) ')
    #browser()

    print('The difference range is:')
    print(range(x1-x2,na.rm=T))
    print('Main cause of difference should be in interpolating the unit hydrograph & rounding of the runoff')
    print('Total outflow in (R, HMS) and their ratio is')
    print( c(sum(x1,na.rm=T)*wawa_dt, sum(x2,na.rm=T)*wawa_dt, sum(x1,na.rm=T)/sum(x2,na.rm=T)))
    print('Total inflow was ')
    print( sum(rv[,2]))
    #if(max(abs(round(x1,2)-x2),na.rm=T)==0.){
    #    print('PASS: No difference when rounded to 2 decimal places')
    #}else{
    #    print('FAIL: Error is beyond round-off error')
    #}

}

##############################################################################################

#'  Compute runoff volume using the SCS runoff model
#'
#'  See page 37+, hec-hms manual. For a comparison with hec-hms, see test_SCS_runoff()
#'
#'   
#'  @param time_increment time over which precip_depth is measured (s)
#'  @param precip_depth vector of 'depth of rainfall' (mm)
#'  @param CN curve number
#'  @param Area catchment area (km^2)
#'  @param dt time step over which to interpolate the data before starting. dt is used to be compatible with hec-hms
#'  @param initial_abstraction_coef Coefficient for initial abstraction
#'  @return 2-column matrix with 'time', 'incremental runoff volume' (in m^3 per dT -- like HMS)
#'  @export
#  #@example
#    # This function does some testing
#    # test_SCS_runoff()
SCS_runoff_volume<-function(time_increment, precip_depth, 
                            CN=74, Area = 1., dt=time_increment,
                            initial_abstraction_coef = 0.2){

    # SCS parameters
    S=(25400 - 254*CN)/CN
    Ia = initial_abstraction_coef*S

    # Approximate precipitation for consistency with HMS.
    #precip_depth=spline( seq(0, (length(precip_depth)-1)*time_increment, by=time_increment),
    #                     precip_depth,
    #                     xout=seq(0, (length(precip_depth)-1)*time_increment, by=dt)
    #                     )$y
    precip_depth=approx( seq(0, (length(precip_depth))*time_increment, by=time_increment),
                            c(precip_depth, precip_depth[length(precip_depth)]),
                            xout=seq(0, (length(precip_depth))*time_increment, by=dt),
                            method='constant'
                            )$y
    precip_depth=precip_depth*dt/time_increment

    # Cumulative precipitation depth in mm
    cum_precip_depth= cumsum(precip_depth)  
    cum_precip_time=seq(0, (length(precip_depth))*dt, by=dt)

    # Excess precipitation depth in mm
    Pe = (cum_precip_depth - Ia)**2*(cum_precip_depth>Ia) / (cum_precip_depth - Ia + S)

    # Incremental_excess * area (m^3)
    dif_Pe = diff(Pe)/1000. * Area * 1000*1000
    lpe=length(dif_Pe)

    # Return a 'time' / incremental excess curve, like hec-hms, but in m^3 units
    return(cbind(seq(0, lpe)*dt, c(0, dif_Pe[1], dif_Pe[1:(lpe-1)])) )
    #return(cbind(seq(0, lpe)*dt, c(0, dif_Pe[1:(lpe)])) )
}


#' Filter incremental rainfall excess through SCS unit hydrograph. 
#'
#' Dimensionless unit hydrograph is based on data from
#' http://www.nohrsc.noaa.gov/technology/gis/uhg_manual.html
#' For a comparison with hec-hms, see test_SCS_uh()

#' @param diff_Pe time & Incremental rainfall excess (s, m^3) [output of runoff_volume_SCS]
#' @param t_lag lag for unit hydrograph (in seconds)
#' @return time-series with 'time' (s), 'discharge' (m^3/s)
#' @export
# @example
#     test_SCS_uh()
#
SCS_uh<-function( diff_Pe, t_lag=3600){

    # Read data -- obtained from 
    # http://www.nohrsc.noaa.gov/technology/gis/uhg_manual.html
    # Data is consistent with other reports I have found.
    #SCS_dimensionless_curve=read.table('scs_dimensionless_unit_hydrograph.txt', header=T)
    SCS_dimensionless_curve=SCS_curve_data()
   
    # Make smooth function to interpolate q_curve 
    # The choice of spline will effect the results, so we can't get exactly the
    # same answer as hec-hms. However, errors seem very small.
    #SCS_qcurve=splinefun(SCS_dimensionless_curve[,1], SCS_dimensionless_curve[,2])
    SCS_qcurve=approxfun(SCS_dimensionless_curve[,1], SCS_dimensionless_curve[,2])

    dT=diff_Pe[2,1]-diff_Pe[1,1] # The time-step in the output data

    # Make dimensionless time vector with even spacing
    t_d=seq(0,5,by=dT/t_lag)
    # Get ordinates for this curve
    SCS_2=SCS_qcurve(t_d) 

    ll=length(t_d)
    # Scale so it integrates to 1.0, using 'real' time as the x-values
    scale_factor=sum(SCS_2[1:ll-1] + SCS_2[2:ll])*(0.5*(t_d[2]-t_d[1])*t_lag)

    SCS_2=SCS_2/scale_factor

    ## Now interpolate the diff_Pe series to the same time interval

    #flow_out=filter(diff_Pe[,2], SCS_2, sides=1, init=rep(0, length(SCS_2)))
    # FIXME: 'init' seems to have no effect on the above code
    flow_out=filter(diff_Pe[,2], SCS_2, sides=1)
    flow_out[1:(length(SCS_2)-1)] = 0. 

    return(cbind(diff_Pe[,1], flow_out))
}

#' SCS dimensionless unit hydrograph data
#' 
#' This holds the SCS UH data 
#'
#' @return The hydrograph 
#' @export
SCS_curve_data<-function(){
    # This is a trick to hold data that should be in a file, in this script
    # FIXME: Make a package and you won't have to do this
    # The character string is written to a file and read.

    mytext=" Time(tpeak), q_curve, mass_curve
    0.0, 0.000, 0.000
    0.1, 0.030, 0.001
    0.2, 0.100, 0.006
    0.3, 0.190, 0.012
    0.4, 0.310, 0.035
    0.5, 0.470, 0.065
    0.6, 0.660, 0.107
    0.7, 0.820, 0.163
    0.8, 0.930, 0.228
    0.9, 0.990, 0.300
    1.0, 1.000, 0.375
    1.1, 0.990, 0.450
    1.2, 0.930, 0.522
    1.3, 0.860, 0.589
    1.4, 0.780, 0.650
    1.5, 0.680, 0.700
    1.6, 0.560, 0.751
    1.7, 0.460, 0.790
    1.8, 0.390, 0.822
    1.9, 0.330, 0.849
    2.0, 0.280, 0.871
    2.2, 0.207, 0.908
    2.4, 0.147, 0.934
    2.6, 0.107, 0.953
    2.8, 0.077, 0.967
    3.0, 0.055, 0.977
    3.2, 0.040, 0.984
    3.4, 0.029, 0.989
    3.6, 0.021, 0.993
    3.8, 0.015, 0.995
    4.0, 0.011, 0.997
    4.5, 0.005, 0.999
    5.0, 0.000, 1.000"
    
    # Write to temporary file
    #cat(mytext, file='.13321scscurve.txt')

    # Read the file
    scs_data=read.table(text=mytext, sep=",", header=T)

    return(scs_data)
}

