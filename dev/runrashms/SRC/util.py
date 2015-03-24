"""
Various convenience functions

"""
import os
import datetime

def make_timestamped_directory(base_directory, prefix):
    """Make a directory named 'prefix9999999999' inside base_directory
    Here the 9's are a date-time-stamp

    It's important there are no underscores in the name (since some of
    our hec-hms tricks will fail)

    @return the path of the new directory

    """

    timestamp = str(datetime.datetime.now()).split('.')[0]
    timestamp = timestamp.replace('-', '')
    timestamp = timestamp.replace(':', '')
    timestamp = timestamp.replace(' ', '')

    newdir = os.path.join(base_directory, prefix + timestamp)

    try:
        os.makedirs(newdir)
    except:
        msg = 'Failed to make directory ' + newdir
        raise Exception(msg)

    return newdir
