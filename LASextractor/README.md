LASextractor
============

R package for reading lidar data from las files and extracting various subsets of interest.

For example it can be used to semi-interactively extract LIDAR point elevations along a road or levee.

This code is sporadically developed based on the needs of the developers and
colleagues. The open source release is mainly to assist with our own management
of the code, as well as to allow others to use it. Bugfixes/suggestions are
nonetheless welcome. 

Installation requires that:
---------------------------

- You have the programs 'las2txt' and 'lasinfo' in your search path. These are freely available at
http://www.cs.unc.edu/~isenburg/lastools/ 

- You also need to have the unstructInterp R package installed. This is not on
  CRAN but installation instructions are on github:
https://github.com/GeoscienceAustralia/unstructInterp

- You also need the rgdal, rgeos and raster packages installed, which can usually be done from inside R

    install.packages(c('rgdal', 'rgeos', 'raster'))

- If you've followed the above steps you will also have the devtools package,
  and can finally install LASextractor from inside R with:

    library(devtools)

    install_github('GeoscienceAustralia/flowtools/LASextractor/LASextractor')

Getting started:
----------------
Look at the help
    
    library(LASextractor)
    ?LASextractor

The example for 'manuallyProjectLasElevationsAlongLine' shows how to use it to
interactively define an elevation profile along a user-provided xy line
shapefile (where elevations are chosen based on elevations of nearby lidar
points).


Bugs
----

If you'd like to contribute any routines or fix bugs, consider forking the
code, inserting your changes and sending the maintainer a pull request.
Otherwise bug reports / suggestions can be sent to the maintainer Gareth
Davies, grothered at gmail dot com
