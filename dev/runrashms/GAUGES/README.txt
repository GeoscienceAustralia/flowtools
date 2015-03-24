%% INSTRUCTIONS ON RAIN + STAGE GAUGE FILE FORMAT
-------------------------------------------------

- The rain/stage gauge data file should be saved in .csv format (the program cannot read .xls or .xlsx)

- At the top of the file you can have comment lines. These start in the first column with '%', and are ignored by the program (except the header lines mentioned below)

- There are also 2 header lines starting with % which tell the code which gauges are rain, and which are stage. They look like:
"%% Rainfall gauge FLAG (1 = TRUE, 0 = FALSE)",,1,1,1,1,1,0,0
"%% Stage gauge FLAG (1 = TRUE, 0 = FALSE)",,0,0,0,0,0,1,1

    - 1's in the Rainfall gauge FLAG row correspond to rainfall data
    - 1's in the Stage gauge FLAG row correspond to stage data
    - Each gauge needs a 1 and a 0 in each row.
    - The code will complain if there are multiple 1's

- The file header line (just above the data) has Day, Time in the first 2 columns, and then the station names.

- The station names must be exactly the same as those in the HECHMS project

- All rainfalls are in mm/hr, under the corresponding station column

- All stages are in m, with the same vertical datum as your hecras model

- The time-interval must be hourly between consecutive rows in the data

- The simulation will run for the duration of the data in the file

- The Day is represented as dd_Mon_YY where dd is a 2 digit day (e.g. 02 or 27), Mon is the month (e.g. Jan, Feb, Mar, Apr, … Dec) and YY is a 2 digit year (e.g. 15). For example 26_Oct_2013 is a valid date

- The time is represented as HH:MM where HH is a 2 digit hour (e.g. 14 or 02 or 00), and MM is a 2 digit minute. The time intervals must be hourly


Below is an example of what the file might look like, for a hypothetical event starting 25 September 2065:
-------------------------------------------------------------------------------------------

%% INSTRUCTIONS on this file format can be found in the README.txt in this directory,,,,,,,,
,,,,,,,,
"%% Rainfall gauge FLAG (1 = TRUE, 0 = FALSE)",,1,1,1,1,1,0,0
"%% Stage gauge FLAG (1 = TRUE, 0 = FALSE)",,0,0,0,0,0,1,1
,,,,,,,,
Day      , Time ,Aries,Boso_Boso,Mt_Campana,Mt_Oro,Science_garden,Manila_Bay,Laguna_Lake
25_Sep_65, 00:00,0,0,0,0,0,11,12.5
25_Sep_65, 01:00,0,0,0,0,0,11.03,12.51
25_Sep_65, 02:00,0,0,0,0,0,11.06,12.52
25_Sep_65, 03:00,0,0,0,0,0,11.09,12.53
25_Sep_65, 04:00,0,0,0,0,0,11.12,12.54
25_Sep_65, 05:00,0,0,0,0,0,11.15,12.55
25_Sep_65, 06:00,0,0,0,0,0,11.18,12.56
25_Sep_65, 07:00,0,0,0,0,0,11.21,12.57
25_Sep_65, 08:00,0,0,0,0,0,11.24,12.58
25_Sep_65, 09:00,0,0,0,0,0,11.27,12.59
25_Sep_65, 10:00,0,0,0,0,0,11.3,12.6
25_Sep_65, 11:00,0,0,0,0,0,11.33,12.61
25_Sep_65, 12:00,0,0,0,0,0,11.36,12.62
25_Sep_65, 13:00,0,0,0,0,0,11.39,12.63
25_Sep_65, 14:00,0,0,0,0,0,11.4,12.64
25_Sep_65, 15:00,0,0,0,0,0,11.4,12.65
25_Sep_65, 16:00,0,0,0,0,0,11.4,12.66
25_Sep_65, 17:00,0,0,0,0,0,11.4,12.67
25_Sep_65, 18:00,2,2,0,4,0,11.4,12.68
25_Sep_65, 19:00,7,17,0,14,2,11.4,12.69
25_Sep_65, 20:00,2,2,0,3,1,11.4,12.7
25_Sep_65, 21:00,11,6,0,6,4,11.4,12.71
25_Sep_65, 22:00,1,1,0,0,6,11.4,12.72
25_Sep_65, 23:00,1,1,0,0,5,11.4,12.73
26_Sep_65, 00:00,11,10,0,16,6,11.4,12.74
26_Sep_65, 01:00,19,15,0,22,8,11.4,12.75
26_Sep_65, 02:00,19,16,0,24,12,11.4,12.76
------------------------------------------------------------------------
