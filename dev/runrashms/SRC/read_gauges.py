"""

Read the gauge data

"""
import csv, re

def check_rain_data_and_convert_to_float(new_rain_data, gauge_file):
    """Check that the rain gauge data has the correct header, 
    and convert the rain rates from string to float

    """

    # Check the header

    msg = 'The first line from the rain gauge data ' + gauge_file +\
          ' is \n ' + str(new_rain_data[0]) + '\n' + \
          'ignoring blank lines and comments starting with % \n' + \
          'This is incorrect. It should begin with "Day" in the first ' +\
          'column, then "Time" in the second column, then the station names'

    if ((new_rain_data[0][0] != 'Day') | (new_rain_data[0][1] != 'Time')):
        raise Exception(msg)

    # Ensure that each station contains numeric data

    for i in range(2, len(new_rain_data[0])):
        if new_rain_data[0][i] != '':
            for j in range(1, len(new_rain_data)):
                try:
                    x = float(new_rain_data[j][i])
                    new_rain_data[j][i] = x
                except:
                    msg = ' The following data appears in column ' + new_rain_data[0][i] +\
                          ' , row ' + str(j) + ' : \n' +\
                          str(new_rain_data[j][i]) + '\n' +\
                          ' It could not be interpreted as a number. Check and correct the file'
                    raise Exception(msg)


    # Check the date format, and convert it to the hecras format
    date_format = re.compile('[0-9][0-9]_[A-Z][a-z][a-z]_[0-9][0-9]')
    short_date_format = re.compile('[0-9]_[A-Z][a-z][a-z]_[0-9][0-9]')
    for i in range(1, len(new_rain_data)):
        if date_format.match(new_rain_data[i][0]) is None:
            if short_date_format.match(new_rain_data[i][0]) is None:
                msg = 'Date ' + str(new_rain_data[i][0]) + ' in row ' +\
                      str(i) + ' in file ' + gauge_file +\
                      ' does not match the format dd_Mon_YY. Fix it'
                raise Exception(msg)
            else:
                new_rain_data[i][0] = '0' + new_rain_data[i][0]

        # Remove underscores -- this converts to HEC HMS date format
        new_rain_data[i][0] = new_rain_data[i][0].replace('_','')
        
    # Ensure the time is of the form [0-9][0-9]:[0-9][0-9]. If it is
    # of the form [0-9]:[0-9][0-9], then adjust by appending a zero
    time_format = re.compile('[0-9][0-9]:[0-9][0-9]')
    short_time_format = re.compile('[0-9]:[0-9][0-9]')
    for i in range(1, len(new_rain_data)):
        if time_format.match(new_rain_data[i][1]) is None:
            if short_time_format.match(new_rain_data[i][1]) is not None:
                new_rain_data[i][1] = '0' + new_rain_data[i][1]

    # Now check the time format
    time_format = re.compile('[0-9][0-9]:[0-9][0-9]')
    final_2_time_digits = new_rain_data[1][1][-2:]
    for i in range(1, len(new_rain_data)):
        if time_format.match(new_rain_data[i][1]) is None:
            msg = 'Time ' + str(new_rain_data[i][1]) + ' in row ' +\
                    str(i) + ' in file ' + gauge_file +\
                    ' does not match the format HH:MM. Fix it'
            raise Exception(msg)
        if new_rain_data[i][1][-2:] != final_2_time_digits:
            msg = 'Time data ' + new_rain_data[i][1] + ' in row ' +\
                    str(i) + ' in file ' + gauge_file +\
                    ' does not have the same minutes as the first data entry' +\
                    ' This is not allowed, since the data MUST be hourly'
            
    return new_rain_data



def read_rain_gauges(gauge_file):
    """Read the rain gauge data in gauge_file

    """

    # Read the csv
    print 'Reading ' + gauge_file
    fid = open(gauge_file, 'r')
    csv_reader = csv.reader(fid)
    rain_gauge_data = [ row for row in csv_reader ]
    fid.close()

    # Remove comment lines and empty lines
    comment_lines = []
    new_rain_data = []
    for i in range(len(rain_gauge_data)):
        if rain_gauge_data[i][0].startswith('%'):
            comment_lines.append(i)
        elif rain_gauge_data[i][0] == '':
            comment_lines.append(i)
        else:
            new_rain_data.append(rain_gauge_data[i])

    # Remove leading and trailing whitespace in file
    for i in range(len(new_rain_data)):
        new_line = [ head.lstrip(' ') for head in new_rain_data[i] ]
        new_line = [ head.rstrip(' ') for head in new_line ]
        new_rain_data[i] = new_line

    new_rain_data = check_rain_data_and_convert_to_float(new_rain_data, 
        gauge_file)

    return new_rain_data


def read_rain_gauges_and_waterlevels(gauge_file):
    """Read rain gauge data and waterlevels from the input file

    Compared to a file with just rainfall, this has more header information,
    saying which stations are rainfall gauges and which are stage gauges

    However, the format is similar enough most of the read / check can be done
    with the rainfall routine

    @param gauge_file csv file containing the data
        
    """

    # First read all the data (as though it is just rainfall)
    raw_file_data = read_rain_gauges(gauge_file)

    # Now search for header information
    rain_gauge_sites_match = re.compile('Rainfall gauge FLAG')
    stage_gauge_sites_match = re.compile('Stage gauge FLAG')

    # Read the csv again
    fid = open(gauge_file, 'r')
    csv_reader = csv.reader(fid)
    gauge_data = [ row for row in csv_reader ]
    fid.close()

    rainfall_header_line = -1
    stage_header_line = -1
    for i, line in enumerate(gauge_data):
        if rain_gauge_sites_match.search(line[0]) is not None:
            print 'Found "Rainfall gauge FLAG" in the file ...'
            rainfall_header_line = i
        elif stage_gauge_sites_match.search(line[0]) is not None:
            print 'Found "Stage gauge FLAG" in the file ...'
            stage_header_line = i



    if not ( (rainfall_header_line >= 0) &\
             (stage_header_line >= 0)):
        print 'WARNING: Did not find rainfall and stage header lines in the \
              file. Assuming they are all rainfalls ...'
    else:
        # Separate the rain from the stage
        rainfall_line = gauge_data[rainfall_header_line]
        stage_line = gauge_data[stage_header_line]

        rainfall_inds = []
        stage_inds = []
        print ''
        for i in range(2, len(rainfall_line)):
            # Sometimes excel can append a blank column to the file
            # Ignore it
            if (rainfall_line[i] == "") and (stage_line[i] == ""):
                continue
            # 
            if int(rainfall_line[i]) == 1:
                print '    ' + raw_file_data[0][i] + ' is RAINFALL'
                rainfall_inds.append(i)
                if int(stage_line[i]) == 1:
                    msg = 'Gauge is also STAGE: Cannot be both'
                    raise Exception(msg)
            elif int(stage_line[i]) == 1:
                print '    ' + raw_file_data[0][i] + ' is STAGE'
                stage_inds.append(i)
                if int(rainfall_line[i]) == 1:
                    msg = 'Gauge is also RAIN: Cannot be both'

        # Make rainfall data and stage data separately
        rain_data = []
        stage_data = []
        for i, line in enumerate(raw_file_data):
            new_rain_data_line = line[0:2] 
            for j in rainfall_inds:
                new_rain_data_line.append(line[j])
            rain_data.append(new_rain_data_line)

            new_stage_data_line = line[0:2]
            for j in stage_inds:
                new_stage_data_line.append(line[j])
            stage_data.append(new_stage_data_line)

        output = [ rain_data, stage_data]
        return output
