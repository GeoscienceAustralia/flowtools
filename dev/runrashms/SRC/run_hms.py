import os, shutil, re, glob

# To get config, we hack
import sys
sys.path.append('..')
import config

###############################################################################
def make_hms_batch_commands(
        hec_hms_project_directory, 
        hec_hms_project_name, 
        hec_hms_simulation_run):
    """Make a file containg batch-script commands for hec-hms

        This is passed to the 'HEC-HMS.cmd -s ' program

        An example of a valid file is:

        ---------------------------------------------------------------------------
        from hms.model.JythonHms import *

        OpenProject("pasigmarikina", "C:\EarlyWarning\MODELS\HECHMS\pasigmarikina")

        Compute("Forecast")

        Exit(1)
        ----------------------------------------------------------------------------

        @return The name of the file (it's location is determined by config.py)

    """

    # Note we insert some 'r' to make a raw string in the file. This seems
    # to be correctly interpreted
    line2 = r'OpenProject(r"' + hec_hms_project_name + r'", r"' +\
            hec_hms_project_directory + r'")'

    line3 = r'Compute("' + hec_hms_simulation_run + r'")'

    hms_batch_commands = []
    hms_batch_commands.append(r'from hms.model.JythonHms import *')
    hms_batch_commands.append(line2)
    hms_batch_commands.append(line3)
    hms_batch_commands.append('Exit(1)')

    # Save to a file

    batch_commands_file = os.path.join(config.TEMPDIRECTORY, 
        config.hms_batch_commands_file)
    fid = open(batch_commands_file, 'w')
 
    for i in range(len(hms_batch_commands)):
        fid.write(hms_batch_commands[i] + '\n')

    fid.close()

    return batch_commands_file

###############################################################################

def run_hms_model(
    hec_hms_project_directory, 
    hec_hms_project_name, 
    hec_hms_simulation_run):
    """
        Run a hec-hms model in batch mode

        @return 0 if the model ran successfully, another value otherwise

    """

    working_directory = os.getcwd()

    if not os.path.exists(config.HEC_HMS_INSTALL_DIRECTORY):
        msg = 'HEC_HMS_INSTALL_DIRECTORY can not be found. \n' +\
              'Make sure the path is correct, and is written as \n' +\
              'a raw string, which begins with r" and ends with " '
        raise Exception(msg)

    # Move to the HEC HMS directory so we can run HMS
    os.chdir(config.HEC_HMS_INSTALL_DIRECTORY)

    batch_commands_file = make_hms_batch_commands(
        hec_hms_project_directory, 
        hec_hms_project_name, 
        hec_hms_simulation_run)

    HMS_command = r"HEC-HMS.cmd -s " + batch_commands_file

    command_output = os.system(HMS_command)

    if command_output != 0: 
        print ''
        print '##############################################'
        print 'ERROR'
        print '##############################################'
        print 'FAILED TO RUN HEC HMS'
        print 'Examine the output above for more information'
        print 'Make sure the project is closed (we expect failure if it is open)'
        raise Exception('FAILED TO RUN HEC HMS')

    else:
        print ''
        print '##############################################'
        print 'SUCCESSFULLY RAN HEC HMS '
        print '##############################################'
        print ''


    os.chdir(working_directory)

    return command_output

###############################################################################

def expand_date(ddMonYY, in1900s=False):
    """Convert a date in ddMonYY format to a list with
    day, Month, Year where 'day' is an integer, 'Month' is the full month name,
    and Year is a 4 digit year
    
    """
    expand_month = {'Jan': 'January',
                    'Feb': 'February',
                    'Mar': 'March',
                    'Apr': 'April',
                    'May': 'May',
                    'Jun': 'June',
                    'Jul': 'July',
                    'Aug': 'August',
                    'Sep': 'September',
                    'Oct': 'October',
                    'Nov': 'November',
                    'Dec': 'December'}

    day = int(ddMonYY[0:2])
    month = expand_month[ddMonYY[2:5]]

    yr = int(ddMonYY[-2:])

    if not in1900s:
        year = 2000 + yr
    else:
        year = 1900 + yr

    return [day, month, year]

###############################################################################

def copy_template_hms_project_and_update_for_forecast(
    new_hms_project_directory, 
    template_hms_project_directory,
    rain_data):
    """Copy from the template hec hms project and update files to reflect the
    new forecast + rainfall dates

        @param new_hms_project_directory The directory to copy to. Created if
        it doesn't exist

        @param template_hms_project_directory The directory to copy from

        @param rain_data The rain data (from which we extract the start and end
        date/time)

        @return Nothing, but copies the key files

    """

    shutil.copytree(template_hms_project_directory, 
        new_hms_project_directory)

    # Need to edit the times of Forecast.control, and the times of the rain
    # gauge data
    start_date = rain_data[1][0]
    end_date = rain_data[-1][0]
    start_hour = rain_data[1][1]
    end_hour = rain_data[-1][1]

    # We need the dates in an expanded format for some things
    start_day, start_month, start_year = expand_date(start_date)
    end_day, end_month, end_year = expand_date(end_date)

    long_start_date = str(start_day) + ' ' + str(start_month) + ' ' +\
        str(start_year)
    long_end_date   = str(end_day)   + ' ' + str(end_month)   + ' ' +\
        str(end_year)

    ###########################################################################
    # Fix the Forecast.control file dates

    forecast_control_file = open(os.path.join(new_hms_project_directory, 
        'Forecast.control'), 'r+')

    forecast_control_lines = forecast_control_file.read().splitlines(True)

    match_start_date = re.compile('Start Date:')
    for i in range(len(forecast_control_lines)):
        if match_start_date.search(forecast_control_lines[i]) is not None:
            forecast_control_lines[i] = '    Start Date: ' + long_start_date + '\n'

    match_start_time = re.compile('Start Time:')
    for i in range(len(forecast_control_lines)):
        if match_start_time.search(forecast_control_lines[i]) is not None:
            forecast_control_lines[i] = '    Start Time: ' + start_hour + '\n'

    match_end_date = re.compile('End Date:')
    for i in range(len(forecast_control_lines)):
        if match_end_date.search(forecast_control_lines[i]) is not None:
            forecast_control_lines[i] = '    End Date: ' + long_end_date + '\n'

    match_end_time = re.compile('End Time:')
    for i in range(len(forecast_control_lines)):
        if match_end_time.search(forecast_control_lines[i]) is not None:
            forecast_control_lines[i] = '    End Time: ' + end_hour + '\n'

    # Rewind the file
    forecast_control_file.seek(0)

    for line in forecast_control_lines:
        forecast_control_file.write(line)

    forecast_control_file.close()


    ###########################################################################
    # Fix the gage start and end times

    gage_control_file = glob.glob(new_hms_project_directory + '/*.gage')
    if len(gage_control_file) != 1:
        msg = 'Did not find a unique gage file in the hms \n' +\
            ' project directory. Found these files: \n' +\
            str(gage_control_file) + '\n' +\
            ' Make sure there is only one such file'
        raise Exception(msg)

    gage_control_file = open(gage_control_file[0], 'r+')

    gage_control_lines = gage_control_file.read().splitlines(True)

    # Need to fix the start/end times for each gage site
    match_start_time = re.compile('     Start Time:')
    match_end_time   = re.compile('     End Time:')
    for gauge_site in rain_data[0][2:]:

        change_next = False

        for i in range(len(gage_control_lines)):

            if gage_control_lines[i] == ('Gage: ' + gauge_site + '\n'):
                # Find the next start/end times
                change_next = True

            if change_next:
                if match_start_time.search(gage_control_lines[i]) is not None:
                    gage_control_lines[i] = '     Start Time: ' +\
                        long_start_date + ', ' + start_hour + '\n'

                if match_end_time.search(gage_control_lines[i]) is not None:
                    gage_control_lines[i] = '     End Time: ' +\
                        long_end_date + ', ' + end_hour + '\n'
                    break

            if i == (len(gage_control_lines)-1):
                msg = 'ERROR: Did not change times in gage ' + gauge_site +\
                    '\n' + 'Make sure the gauge_site name in the rainfall file\
                    is the same as in the HEC-HMS project'
                raise Exception(msg)

    # Now the gauge_control_lines should be fixed
    gage_control_file.seek(0)

    for line in gage_control_lines:
        gage_control_file.write(line)

    gage_control_file.close()

    return 


