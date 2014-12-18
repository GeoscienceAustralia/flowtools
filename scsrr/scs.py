"""

Implementation of the SCS loss model + SCS unit hydrograph

"""
import numpy
import scipy
import scipy.interpolate

# SCS dimensionless unit hydrograph data
SCS_DIMENSIONLESS_UH = numpy.array([
    #" Time(tpeak), q_curve, mass_curve
    [0.0, 0.000, 0.000],
    [0.1, 0.030, 0.001],
    [0.2, 0.100, 0.006],
    [0.3, 0.190, 0.012],
    [0.4, 0.310, 0.035],
    [0.5, 0.470, 0.065],
    [0.6, 0.660, 0.107],
    [0.7, 0.820, 0.163],
    [0.8, 0.930, 0.228],
    [0.9, 0.990, 0.300],
    [1.0, 1.000, 0.375],
    [1.1, 0.990, 0.450],
    [1.2, 0.930, 0.522],
    [1.3, 0.860, 0.589],
    [1.4, 0.780, 0.650],
    [1.5, 0.680, 0.700],
    [1.6, 0.560, 0.751],
    [1.7, 0.460, 0.790],
    [1.8, 0.390, 0.822],
    [1.9, 0.330, 0.849],
    [2.0, 0.280, 0.871],
    [2.2, 0.207, 0.908],
    [2.4, 0.147, 0.934],
    [2.6, 0.107, 0.953],
    [2.8, 0.077, 0.967],
    [3.0, 0.055, 0.977],
    [3.2, 0.040, 0.984],
    [3.4, 0.029, 0.989],
    [3.6, 0.021, 0.993],
    [3.8, 0.015, 0.995],
    [4.0, 0.011, 0.997],
    [4.5, 0.005, 0.999],
    [5.0, 0.000, 1.000]
])

##############################################################################


def scs_runoff_volume(time_increment, rain_depth, curve_number=74.0,
                      catchment_area=1.0, dt=None,
                      initial_abstraction_coef=0.2):
    """
        Compute the runoff volume by the SCS method,
        given a rainfall depth timeseries, curve number, catchment area

        @param time_increment time over which rain_depth is measured (s)
        @param rain_depth numpy array of 'depth of rainfall'. Each entry occurs
            over time time_increment (mm)
        @param curve_number SCS curve number
        @param catchment_area catchment area (km^2)
        @param dt time step over which to interpolate the rain_depth before
            starting. Used for compatibility with hec-hms. If None then
            time_increment/60 is used
        @param initial_abstraction_coef Initial abstraction Ia =
            initial_abstraction_coef * S. Can adjust to reflect catchment
            initial conditions

        @return 2-column matrix with 'time', 'incremental runoff volume'
             (in m^3 per dt -- like hec-hms)
    """
    if dt is None:
        dt = time_increment / 60.

    # SCS parameters
    S = (25400. - 254. * curve_number) / curve_number
    Ia = initial_abstraction_coef * S

    # Make function to interpolate precipitation timeseries
    rain_time = scipy.linspace(0., time_increment * len(rain_depth),
                               num=len(rain_depth) + 1)
    rain_depth_persecond = scipy.hstack([rain_depth / time_increment,
                                         rain_depth[-1] / time_increment])

    rain_depth_interpfun = scipy.interpolate.interp1d(
        rain_time, rain_depth_persecond, kind='zero')

    # Get the interpolated timeseries
    n_interp = int(numpy.ceil(time_increment * len(rain_depth) / dt + 1))
    time_interp = scipy.linspace(0., time_increment * (len(rain_depth)),
                                 num=n_interp)
    dt_interp = (time_interp[1] - time_interp[0])
    rain_depth_interp = rain_depth_interpfun(time_interp) * dt_interp

    cumulative_rain_depth = scipy.cumsum(rain_depth_interp)

    # Use SCS formula to compute Pe = Excess precipitation depth in mm
    denominator = (cumulative_rain_depth - Ia + S)
    numerator = (cumulative_rain_depth - Ia) ** 2 * \
        (cumulative_rain_depth > Ia)
    Pe = numerator / denominator

    # Compute incremental excess in units m^3 per dt
    incremental_excess = (Pe[1:] - Pe[0:-1]) / 1000. * catchment_area * 1.0e+06
    # Stitch together in this way to agree with HMS interpolation
    # Cause is due to the way rain at e.g. 1hr is interpolated to either the
    # (1-2) hr value, or the (0-1) hr value
    incremental_excess = scipy.hstack([numpy.zeros(1), incremental_excess[0],
                                       incremental_excess[0:-1]])

    output_time = scipy.linspace(0., dt_interp * (len(incremental_excess) - 1),
                                 num=len(incremental_excess))

    output = numpy.vstack([output_time, incremental_excess]).transpose()

    return output

###############################################################################


def scs_unit_hydrograph(runoff, lag_time):
    """

        Filter incremental rainfall excess through SCS unit hydrograph. 

        Dimensionless unit hydrograph is based on data from
        http://www.nohrsc.noaa.gov/technology/gis/uhg_manual.html

        @param runoff time & Incremental rainfall excess (s, m^3) 
            [output of scs_runoff_volume]
        @param lag_time lag for unit hydrograph (in seconds)
        @return time-series with 'time' (s), 'discharge' (m^3/s)

    """

    dt = runoff[1, 0] - runoff[0, 0]

    msg = 'runoff time spacing must be constant'
    assert numpy.allclose(numpy.diff(runoff[:, 0]), dt), msg

    msg = 'dt must be <= lag_time'
    assert dt <= lag_time, msg

    scs_q_curve = scipy.interpolate.interp1d(SCS_DIMENSIONLESS_UH[:, 0],
                                             SCS_DIMENSIONLESS_UH[:, 1],
                                             kind='linear')

    # Make dimensionless time vector with even spacing
    t_d = numpy.arange(0., 5., dt / float(lag_time))
    scs2 = scs_q_curve(t_d)

    # Ensure unit hydrograph integrates to 1.0
    scale_factor = (
        (scs2[0:-1] + scs2[1:]) * (0.5 * (t_d[1] - t_d[0]) * lag_time)).sum()
    scs2 = scs2 / scale_factor

    # Do convolution
    # Pad filter so that it has odd length
    zero_pad = numpy.zeros(len(scs2) - 1)
    filt = numpy.hstack([zero_pad, scs2])
    # Pad series to remove edge effects
    padded_series = numpy.hstack([zero_pad, runoff[:, 1], zero_pad])
    output_discharge = numpy.convolve(padded_series, filt, mode='valid')
    output = numpy.vstack([runoff[:, 0], output_discharge]).transpose()

    return output
