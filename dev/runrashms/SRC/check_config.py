"""

Check for typical problems with config.py variables

"""

import sys

# Hack to get config
import sys
sys.path.append('..')
from config import *


###############################################################################
#
# TEST FOR PROBLEMATIC FILE NAMES. 
#
###############################################################################

# Files exist
import os

msg = 'Could not find HEC_HMS_PROJECT_DIRECTORY ' + HEC_HMS_PROJECT_DIRECTORY
assert os.path.exists(HEC_HMS_PROJECT_DIRECTORY), msg

msg = 'Could not find  HEC_HMS_INSTALL_DIRECTORY ' + HEC_HMS_INSTALL_DIRECTORY
assert os.path.exists(HEC_HMS_INSTALL_DIRECTORY),  msg

msg = 'Could not find  HEC_RAS_PROJECT_FILE ' + HEC_RAS_PROJECT_FILE
assert os.path.exists(HEC_RAS_PROJECT_FILE),  msg

msg = 'Count not find HEC_DSSVUE_INSTALL_DIRECTORY ' +\
      HEC_DSSVUE_INSTALL_DIRECTORY
assert os.path.exists(HEC_DSSVUE_INSTALL_DIRECTORY), msg

msg = 'Count not find HEC_HMS_PROJECT_NAME ' + HEC_HMS_PROJECT_NAME +\
      ' in HEC_HMS_PROJECT_DIRECTORY ' + HEC_HMS_PROJECT_DIRECTORY
sim_hms_file = os.path.join(HEC_HMS_PROJECT_DIRECTORY, 
                            HEC_HMS_PROJECT_NAME + '.hms')
assert os.path.exists(sim_hms_file), msg

msg = 'Could not find HEC_HMS_SIMULATION_RUN file ' + HEC_HMS_SIMULATION_RUN +\
      '.control in HEC_HMS_PROJECT_DIRECTORY ' + HEC_HMS_PROJECT_DIRECTORY +\
      '\n . This file must exist and have been executed at least once \n' +\
      '(with the correct settings, aside from the simulation time) ' +\
      'before running this code.'
control_hms_file = os.path.join(HEC_HMS_PROJECT_DIRECTORY, 
   HEC_HMS_SIMULATION_RUN + '.control')
assert os.path.exists(control_hms_file), msg

# Ensure HECRAS file really is a .prj hecras project file
msg = 'HEC_RAS_PROJECT_FILE must be a .prj file'
assert HEC_RAS_PROJECT_FILE[-4:] == '.prj', msg


# No underscores

msg = 'Cannot have underscores "_" in the HEC_HMS_PROJECT_DIRECTORY name' +\
      ', but found ' + HEC_HMS_PROJECT_DIRECTORY
assert HEC_HMS_PROJECT_DIRECTORY.rfind('_') == -1, msg

msg = 'Cannot have underscores "_" in the TEMPDIRECTORY name, but found ' +\
       TEMPDIRECTORY
assert TEMPDIRECTORY.rfind('_') == -1, msg 

msg = 'Cannot have underscores "_" in the HEC_HMS_PROJECT_NAME'
assert HEC_HMS_PROJECT_NAME.rfind('_') == -1 , msg

msg = 'Cannot have underscores "_" in the HEC_HMS_SIMULATION_RUN'
assert HEC_HMS_SIMULATION_RUN.rfind('_') == -1, msg


# No spaces 

msg = 'Cannot have spaces " " in the HEC_HMS_PROJECT_DIRECTORY name' +\
      ', but found ' + HEC_HMS_PROJECT_DIRECTORY
assert HEC_HMS_PROJECT_DIRECTORY.rfind(' ') == -1, msg

msg = 'Cannot have spaces " " in the TEMPDIRECTORY name, but found ' +\
       TEMPDIRECTORY
assert TEMPDIRECTORY.rfind(' ') == -1, msg 

msg = 'Cannot have spaces " " in the HEC_HMS_PROJECT_NAME'
assert HEC_HMS_PROJECT_NAME.rfind(' ') == -1 , msg

msg = 'Cannot have spaces " " in the HEC_HMS_SIMULATION_RUN'
assert HEC_HMS_SIMULATION_RUN.rfind(' ') == -1, msg

