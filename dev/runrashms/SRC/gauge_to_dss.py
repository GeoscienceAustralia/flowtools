"""

Script to interface with the HEC-DSSVue program, which allows
us to create DSS Files.

The approach is to have a template text file containing a script for the
HEC-DSSVue program. This is edited by python (so key variables are inserted)
and then we call the HEC-DSSVue program.

"""
import os, re

# Hack to get config
import sys
sys.path.append('..')
import config

def edit_gauge2dss_script(dss_script, output_dss_filename, gauge_site, gauge_type,
    gauge_date, start_time, rain_values):
    """Edit the template dss script, so that data from a given gauge_site
    can be added to the output_dss_filename (with date/time/rain provided)

    The output_dss_filename file will be created if it does not already exist

    """

    # Set the filename
    dss_script = dss_script.replace('DSSFILENAME', output_dss_filename)

    # Set the gauge site
    p = re.compile('GAUGESITE')
    dss_script = p.sub(gauge_site, dss_script)
    
    # Set the gauge type
    p = re.compile('PRECIP-INC')
    if gauge_type == 'rain':
        # Already correct
        pass
    elif gauge_type == 'stage':
        dss_script = p.sub('STAGE', dss_script)

    # Set the gauge_date
    p = re.compile('GAUGEDATE')
    dss_script = p.sub(gauge_date, dss_script)

    # Set the gauge_time
    p = re.compile('STARTTIME')
    dss_script = p.sub(start_time, dss_script)
   
    # Insert the rainfall values
    p = re.compile('RAINVALUES')
    dss_script = p.sub(str(rain_values), dss_script)

    # Convert to list, keeping the \n
    dss_script = dss_script.splitlines(True)

    return dss_script


def run_hec_dssvue(dss_script_filename, gauge=''):
    """ Run:

    HEC-DSS.exe dss_script_filename 

    from inside the HEC-DSS install directory

    """

    mydir = os.getcwd()
    os.chdir(config.HEC_DSSVUE_INSTALL_DIRECTORY)

    try:
        # Run hec-dssvue 
        cmd = 'HEC-DSSVue.exe ' + dss_script_filename

        run_result = os.system(cmd)

        if run_result != 0:
            print ''
            print '##############################################'
            print 'ERROR'
            print '##############################################'
            msg = 'Failed to run HEC-DSSVue.exe. Check the output above for information'
            raise Exception(msg)
        else:
            print ''
            print '##############################################'
            print 'RAN HEC-DSSVue.exe for gauge ' + gauge 
            print '##############################################'

    except:
        msg = 'Failed to run HEC-DSSVue.exe'

    finally:
        # Back to the original directory
        os.chdir(mydir)

    return


def make_new_dss_file(rain_data, stage_data, output_dss_filename):
    """Make a dss file containing the rain_data and stage_data

    @param rain_data rain data output from read_gauges.read_rain_gauges_and_waterlevels
    @param stage_data stage data output from read_gauges.read_rain_gauges_and_waterlevels
    @param output_dss_filename The name of the dss file to make
    @return Nothing, but make the dss file

    """
    
    gauge_date = rain_data[1][0]
    start_time = rain_data[1][1]

    # Remove the output_dss_filename if it exists already
    try:
        os.remove(output_dss_filename)
    except:
        pass

    # Get the directory to the template script for HEC_DSSVUE
    this_directory = os.path.dirname(os.path.realpath(__file__))
    hec_dssvue_template_file_creation_script = os.path.join(
        this_directory,
        'GaugeToDssTemplate.txt')

    # Append rain values to the new dss file
    for i in range(2, len(rain_data[0])):
        gauge_site = rain_data[0][i]
        gauge_type = 'rain'

        # Get the template script (which we will modify)
        dss_script = open(hec_dssvue_template_file_creation_script,'r').read()

        # Get the rain values for this station
        rain_values = []
        for j in range(1, len(rain_data)):
            rain_values.append(rain_data[j][i])

        # Insert the information for this gauge_site
        new_dss_script = edit_gauge2dss_script(
            dss_script, 
            output_dss_filename, 
            gauge_site, 
            gauge_type,
            gauge_date, 
            start_time, 
            rain_values)

        # Write the script to a file
        temp_dss_file = os.path.join(
            config.TEMPDIRECTORY, 
            config.temp_dss_script)
        fid = open(temp_dss_file, 'w')
        fid.writelines(new_dss_script)
        fid.close()

        # Call hec_dssvue on the script
        run_hec_dssvue(temp_dss_file, gauge=gauge_site)

    # Append stage values to the new dss file
    # FIXME: This is nearly a copy of the code above -- could use a function
    for i in range(2, len(stage_data[0])):
        gauge_site = stage_data[0][i]
        gauge_type = 'stage'

        # Get the template script (which we will modify)
        dss_script = open(hec_dssvue_template_file_creation_script,'r').read()

        # Get the stage values for this station
        stage_values = []
        for j in range(1, len(stage_data)):
            stage_values.append(stage_data[j][i])

        # Insert the information for this gauge_site
        new_dss_script = edit_gauge2dss_script(
            dss_script, 
            output_dss_filename, 
            gauge_site, 
            gauge_type,
            gauge_date, 
            start_time, 
            stage_values)

        # Write the script to a file
        temp_dss_file = os.path.join(
            config.TEMPDIRECTORY, 
            config.temp_dss_script)
        fid = open(temp_dss_file, 'w')
        fid.writelines(new_dss_script)
        fid.close()

        # Call hec_dssvue on the script
        run_hec_dssvue(temp_dss_file, gauge=gauge_site)

    # Now the temp_dss_file should exist
    return temp_dss_file
