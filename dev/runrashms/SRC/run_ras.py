"""

Run a hecras model from python

"""
import win32com
import win32com.client
import shutil
import os, glob, re

# To get config, we hack
import sys
sys.path.append('..')
import config

from run_hms import expand_date


def run_hecras_win32com(hecras_project_file):
    """
        Run hecras using python's win32com library.
        This doesn't work at present but shows at least some of the steps.
    """

    # Import hecras
    #
    # The following code was suggested after running:
    # > python makevars.py -i 
    # where on my machine, makevars.py lives in /c/Python27/Lib/site-packages/win32com/client
    #
    from win32com.client import gencache
    hecras = gencache.EnsureModule('{D86F4816-095A-4F1E-BAD6-CCA2609EFD8D}', 0, 1, 2)

    # Open the hecras controller
    HRC = hecras.HECRASController()
    # [To learn about the methods, do 
    #   > dir(HRC.default_interface)
    #  For some reason the usual dir interface doesn't work]

    # Open the project
    HRC.Project_Open(hecras_project_file)

    # FIXME: How to get this to work?? Need to pass 2 arguments, not clear what
    HRC.Compute_CurrentPlan() 



def run_hecras_via_excel(excel_hecras_runner_file, hecras_project_file):
    """Run hecras via excel. 

    This is in-elegant -- the win32com method should be nicer -- but I couldn't
    get the latter to work yet.

    @param excel_hecras_runner_file Name of xls file containing VBA code to run hecras.

    @param hecras_project_file Name of the hecras project file (will be inserted in the excel file)

    @return Nothing, but runs hecras
    """
    print ''
    print '###############################################'
    print '#'
    print '# RUNNING HECRAS via EXCEL workbook ' + excel_hecras_runner_file
    print '#'
    print '###############################################'

    xl = win32com.client.Dispatch("Excel.Application")

    #xl.Workbooks.Open(Filename=excel_hecras_runner_file, ReadOnly=1)
    book = xl.Workbooks.Open(Filename=excel_hecras_runner_file, ReadOnly=0)
    book.Worksheets(1).Cells(2,2).Value = hecras_project_file

    xl.Application.Run("RunRAS")
    xl.Workbooks(1).Close(SaveChanges=1)
    xl.Application.Quit()
    xl = 0

    # Do we need this?
    book = 0

    return

###############################################################################

def copy_template_ras_project_and_update_for_forecast(
    new_ras_project_directory, 
    template_ras_project_directory,
    rain_data):

    """Copy from the template hec ras project and update files to reflect the
    new forecast + rainfall dates

        @param new_ras_project_directory The directory to copy to. Created if
        it doesn't exist

        @param template_ras_project_directory The directory to copy from

        @param rain_data The rain data from the simulation (from which
        we extract the start/end times)

        @return Nothing, but copies and edits the key files

    """

    shutil.copytree(template_ras_project_directory, 
        new_ras_project_directory)

    # Also get the excel worksheet to run ras
    shutil.copy(config.EXCEL_HEC_RAS_RUNNER_FILE,
        os.path.dirname(new_ras_project_directory))

    excel_runner_file = os.path.join(
        os.path.dirname(new_ras_project_directory),
        os.path.basename(config.EXCEL_HEC_RAS_RUNNER_FILE))

    # Modify the simulations times in the hecras *.pXX file, where XX are integers

    dot_p_file = glob.glob(new_ras_project_directory + r'/*.p[0-9][0-9]')
    if(len(dot_p_file) != 1):
        msg = 'Did not find exactly one hecras file ending in .pXX \n' +\
              '(where XX are in 0,1,2,..9). ' +\
              'Found ' + str(dot_p_file) + '\n' +\
              'There should only be one .pXX file (where the X are numbers).' +\
              'Ensure there is only one in the template hecras project directory'
        raise Exception(msg)
   
    dot_p_file = dot_p_file[0]

    # Open / read the file
    fid = open(os.path.join(new_ras_project_directory, dot_p_file), 'r+')
    dot_p_lines = fid.read().splitlines(True)

    sim_start_date = expand_date(rain_data[1][0])
    sim_start_hour = rain_data[1][1]
    sim_end_date = expand_date(rain_data[-1][0])
    sim_end_hour = rain_data[-1][1]

    # The date in this file is of the form ddMONYYYY
    new_start_date = rain_data[1][0][:-2] + str(sim_start_date[2])
    new_start_date = new_start_date.upper()
    new_end_date = rain_data[-1][0][:-2] + str(sim_end_date[2])
    new_end_date = new_end_date.upper()

    # The Simulation Date=... line will be replaced with this
    new_sim_date_line = 'Simulation Date=' +\
        new_start_date +  ',' + sim_start_hour + ',' +\
        new_end_date   +  ',' + sim_end_hour + '\n'

    # Change the text
    simulation_date_match = re.compile('Simulation Date')
    for i, line in enumerate(dot_p_lines):
        if simulation_date_match.search(line) is not None:
            dot_p_lines[i] = new_sim_date_line
  
    # Write out the file
    fid.seek(0)
    for line in dot_p_lines:
        fid.write(line)

    fid.close()

    # Change the times in the boundary condition files pointing to DSS paths
    bc_file = glob.glob(new_ras_project_directory + r'/*.u[0-9][0-9]')
    if(len(bc_file) != 1):
        msg = 'Did not find exactly one hecras file ending in .uXX \n' +\
              '(where XX are in 0,1,2,..9). ' +\
              'Found ' + str(bc_file) + '\n' +\
              'There should only be one .uXX file (where the X are numbers)' +\
              'Ensure there is only one in the template hecras project directory'
        raise Exception(msg)
   
    bc_file = bc_file[0]

    # Open / read the file
    fid = open(os.path.join(new_ras_project_directory, bc_file), 'r+')
    bc_lines = fid.read().splitlines(True)
    
    
    # Find all lines giving the names of boundary conditions (for printing
    # only)
    boundary_name_match = re.compile('Boundary Location=')

    # We will have to find all lines starting with DSS Path=//....
    dss_path_match = re.compile('DSS Path=//') 
    empty_dss_path_match = re.compile('DSS Path')
    #...and on those lines we will have to match the date in the form ddMonYYYY
    ddMonYYYY_match = re.compile(
        '[0-9][0-9]' +\
        '(JAN|FEB|MAR|APR|JUN|JUL|AUG|SEP|OCT|NOV|DEC)' +\
        '[0-9][0-9][0-9][0-9]')

    boundary_name = ''

    for i, line in enumerate(bc_lines):

        if boundary_name_match.search(line) is not None:
            boundary_name = line

        if dss_path_match.search(line) is not None:
            # Replace the ddMonYYYY with the new_start_date
            if ddMonYYYY_match.search(line) is None:
                msg = 'Did not match the date in the unsteady flow file on this line: \n'\
                      + line + '\n' + ' for boundary location ' + boundary_name
                raise Exception(msg)
            bc_lines[i] = ddMonYYYY_match.sub(new_start_date, line)
        elif empty_dss_path_match.search(line) is not None:
            print 'WARNING: Not updating the boundary condition at ' +\
                boundary_name

    # Save the file
    fid.seek(0)
    for line in bc_lines:
        fid.write(line)

    fid.close()

    return excel_runner_file
