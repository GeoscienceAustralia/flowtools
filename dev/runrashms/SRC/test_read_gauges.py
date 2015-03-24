import read_gauges
import unittest

class TestReadGauges(unittest.TestCase):

    def setUp(self):
        pass
        return

    def tearDown(self):
        pass
        return

    def test_read_gauges(self):

        # Check that we can successfully read a test input file
        # (at least we don't hit errors in the code, which is reasonably
        # defensive)
        test_file = '../GAUGES/Simulation_rain_and_waterlevel_test.csv'
        rain_data, stage_data = read_gauges.read_rain_gauges_and_waterlevels(test_file)

        # FIXME: Add checks on the rain_data output

        return

if __name__ == '__main__':
    suite = unittest.makeSuite(TestReadGauges, 'test')
    runner = unittest.TextTestRunner()
    runner.run(suite)

