"""
This is the main 'driver' script to run the forecast

"""
import os
import sys
import config
import SRC.check_config
import SRC.read_gauges as read_gauges
import SRC.util as util
import SRC.run_hms as run_hms
import SRC.gauge_to_dss as gauge_to_dss
import SRC.run_ras as run_ras

####################################################
#
# INPUT DATA
#
####################################################

# The user must provide an input file -- it is assumed to be in the GAUGES
# directory whether or not the GAUGES dirname is provided
if len(sys.argv) < 2:
    msg = 'Must provide an input rain-and-waterlevel csv file which is ' +\
          'inside the GAUGES directory'
    raise Exception(msg)
else:
    if os.path.basename(os.path.dirname(sys.argv[1])) == 'GAUGES':
        # Accept the filename since it is in a GAUGES directory
        gauges_file = sys.argv[1]
    else:
        # Assume the file is in the GAUGES directory
        gauges_file = os.path.join('GAUGES',sys.argv[1])

    if not os.path.exists(gauges_file):
        msg = '\nCould not find file ' + gauges_file + '\n'\
              'Make sure there is a file in the GAUGES ' +\
              'directory with the provided filename'
        raise Exception(msg)


####################################################
#
# END INPUT DATA
#
####################################################


## Read the rain and stage gauge data

rain_data, stage_data = read_gauges.read_rain_gauges_and_waterlevels(gauges_file)

## Make a forecast_directory to hold intermediate outputs

forecast_directory = util.make_timestamped_directory(
    base_directory=config.TEMPDIRECTORY,
    prefix=config.HEC_HMS_PROJECT_NAME)

## Copy the hms template project to a new location, and update them

hms_forecast_directory = os.path.join(
    os.path.join(forecast_directory, 'HECHMS'),
    os.path.basename(config.HEC_HMS_PROJECT_DIRECTORY))

run_hms.copy_template_hms_project_and_update_for_forecast(
    hms_forecast_directory, 
    config.HEC_HMS_PROJECT_DIRECTORY,
    rain_data)

## Overwrite the old hms dss file with a new one, containing forecast data

old_dss_filename = os.path.join(hms_forecast_directory, 
    config.HEC_HMS_PROJECT_NAME + '.dss')

new_dss_file = gauge_to_dss.make_new_dss_file(rain_data, stage_data, 
    old_dss_filename)

# Run HEC-HMS with the forecast data
run_hms.run_hms_model(
    hms_forecast_directory, 
    config.HEC_HMS_PROJECT_NAME, 
    'Forecast')

# Copy the HECRAS project, and convert the HEC-HMS outputs to HECRAS boundary
# conditions 
ras_forecast_directory = os.path.join(
    os.path.join(forecast_directory, 'HECRAS'),
    os.path.basename(os.path.dirname(config.HEC_RAS_PROJECT_FILE)))

excel_hecras_runner_file = run_ras.copy_template_ras_project_and_update_for_forecast(
    ras_forecast_directory,
    os.path.dirname(config.HEC_RAS_PROJECT_FILE),
    rain_data)

# Run HEC-RAS
run_ras.run_hecras_via_excel(
    excel_hecras_runner_file, 
    os.path.join(ras_forecast_directory, 
                 os.path.basename(config.HEC_RAS_PROJECT_FILE))
    )

