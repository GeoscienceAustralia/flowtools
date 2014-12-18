import unittest
import numpy
import scs


class Test_scs(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_runoff_volume(self):
        """

        """

        rainfall = numpy.genfromtxt('rainfall_test1.txt', skip_header=1)
        runoff = scs.scs_runoff_volume(3600., rainfall, dt=60.)
        hms_runoff = numpy.genfromtxt('hms_test1.txt', skip_header=1)

        # Adjust computed runoff for hms compatibility (including rescaling to
        # mm * catchment area, + rounding to the number of digits that hms
        # reports)
        runoff_rounded = (runoff[:, 1] / 1000.).round(2)

        error = runoff_rounded - hms_runoff

        assert numpy.allclose(numpy.diff(runoff[:, 0]) - 60., 0.)
        assert len(runoff[:, 1]) == len(hms_runoff)
        assert (error[1:].max() == 0.)
        assert (error[1:].min() == 0.)

        # m^3 rainfall input -- convert from mm to m, and from km^2 to m^2
        rainfall_vol = (rainfall.sum() / 1000.) * 1.0e+06
        # m^3 runoff volume
        runoff_vol = runoff[:, 1].sum()
        assert (rainfall_vol > runoff_vol)

        return

    def test_scs_uh(self):
        """

        """
        rainfall = numpy.genfromtxt('rainfall_test1.txt', skip_header=1)

        wawa_CN = 74.
        wawa_t_lag = 168. * 60
        wawa_area = 287.
        wawa_dt = 60.

        runoff = scs.scs_runoff_volume(3600., rainfall, curve_number=wawa_CN,
                                       catchment_area=wawa_area, dt=wawa_dt)

        outflow = scs.scs_unit_hydrograph(runoff, lag_time=wawa_t_lag)

        hms_outflow = numpy.genfromtxt('hms_test2.txt', skip_header=1)

        difference = outflow[:, 1] - hms_outflow

        # As with R, there are tiny differences probably due to hec-hms
        # interpolation / rounding
        assert abs(difference).max() < 10.0
        import scipy.stats
        assert scipy.stats.pearsonr(outflow[:, 1], hms_outflow)[0] > 0.99999

        return

if __name__ == "__main__":
    suite = unittest.makeSuite(Test_scs, 'test')
    runner = unittest.TextTestRunner()
    runner.run(suite)
