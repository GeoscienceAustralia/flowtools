

# Path to the HEC HMS model used for forecast, as a 'raw' python string
# (There must not be any underscores '_' in the name -- this will make the
# program fail)
HEC_HMS_PROJECT_DIRECTORY = r"C:\EarlyWarning\MODELS\HECHMS\pasigmarikina3"

# Name of the project, as a 'raw' python string
# (There must not be any underscores '_' in the name -- this will make the
# program fail)
HEC_HMS_PROJECT_NAME = r"pasigmarikina3"

# Name of the simulation run
# (There must not be any underscores '_' in the name -- this will make the
# program fail)
HEC_HMS_SIMULATION_RUN = r"Forecast"

# Path to HEC HMS install on your computer, as a 'raw' python string
# (It must be in quotes, and have a 'r' at the start to
# prevent python interpreting backslash as a special character)
HEC_HMS_INSTALL_DIRECTORY = r"C:\Program Files (x86)\HEC\HEC-HMS\3.5"

# Path to HEC DSSVue install on your computer, as a 'raw' python string
# (It must be in quotes, and have a 'r' at the start to
# prevent python interpreting backslash as a special character)
HEC_DSSVUE_INSTALL_DIRECTORY = r"C:\Program Files (x86)\HEC\HEC-DSSVue"

# Name of the directory to write temporary files to. 
# It should exist, not contain any underscores in its name, and you need to
# have permissions to write to it
TEMPDIRECTORY = r"C:\Users\Gareth\Documents\TEMPEarlyWarning"

# Name of an excel file which contains a VBA macro to run hecras
# The excel spreadsheet contains the filename for the hecras project - that's
# what you have to edit if you want to use a different hecras project
EXCEL_HEC_RAS_RUNNER_FILE = r"C:\EarlyWarning\MODELS\HECRAS\RunHecRasVBAcode.xls"

HEC_RAS_PROJECT_FILE = r"C:\EarlyWarning\MODELS\HECRAS\SimpleDraftModel2\marikina.prj"


# The following 'temporary' files will be made in the temp directory
# You probably don't need to change this
hms_batch_commands_file = 'hmsbatchcommands.txt'
temp_dss_script = 'dssbuildscript.py'
temp_dss_file = 'tempdssfile.dss'



