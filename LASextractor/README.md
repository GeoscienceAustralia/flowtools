LASextractor
============

R package for reading lidar data from las files and extracting various subsets
of interest.

For example it can be used to semi-interactively extract LIDAR point elevations
along a road or levee.

Illustration:
-------------
In this case the user has a large LIDAR dataset (consisting of ~ one thousand
.las files, each including points within a separate square kilometre region, with
billions of points in total), and wants to extract elevations along a narrow
drain. The entire dataset is too large to load into memory at once.
LASextractor can help efficiently extract the desired subset.

In Figure 1 below, the user has created a line shapefile along a narrow drain
in a area where LIDAR data exists (that's the narrow pink-ish line which
follows the blue points).

![planview](Drain_GIS_Planview.png?raw=true)

[Figure 1]

LASextractor was used to extract a subset of the LIDAR xyz points within 10m of
the line shapefile, which is small enough to fit easily in memory, and includes
points both inside the drain and near its banks. These point elevations were
plotted vs distance along the line (Figure 2). The user then interactively
defined a line marking the drain invert by clicking on the plot (red dotted
line in Figure 2). Note the drain invert which shows up fairly clearly in the
LIDAR point subset, as a narrow band of points plotting a few metres below most
others.  

![profile](Drain_bedProfile.png?raw=true)

[Figure 2]

Finally all LIDAR points within a vertical distance of 20cm from the line
defined in Figure 2 were exported (see blue points in Figure 1). We can
cross-check in GIS that these xyz points were over the drain invert, which
confirms that the drain invert was accurately interpreted from the data in
Figure 2. We can also export evenly spaced xyz points along the initially
defined line, where the 'z' elevation is computed based on the user-defined
line in Figure 2. Such outputs can then be used in other software [for example,
to define drain elevations for flood modelling].

Naturally the quality of the extracted points is only as good as the input data
accuracy, and will depend on the manually digitized profile (Figure 2). Further
filtering or editing may be required.  Ground-truthing is helpful to confirm
that the extracted elevations do accurately reflect the feature of interest.

Installation requires that:
---------------------------

- You have the programs 'las2txt' and 'lasinfo' in your search path. These are freely available at
http://www.cs.unc.edu/~isenburg/lastools/ 

- You also need to have the unstructInterp R package installed. This is not on
  CRAN but installation instructions are on github:
https://github.com/GeoscienceAustralia/unstructInterp

- You also need the rgdal, rgeos and raster packages installed, which can usually be done from inside R

    ``install.packages(c('rgdal', 'rgeos', 'raster'))``

- If you've followed the above steps you will also have the devtools package,
  and can finally install LASextractor from inside R with:

    ``library(devtools)``

    ``install_github('GeoscienceAustralia/flowtools/LASextractor/LASextractor')``

Getting started:
----------------
Look at the help
    
    library(LASextractor)
    ?LASextractor

The example for ``manuallyProjectLasElevationsAlongLine`` shows how to use it to
interactively define an elevation profile along a user-provided xy line
shapefile (where elevations are chosen based on elevations of nearby lidar
points).


Maintenance and Bugs
---------------------
This code is sporadically developed based on the needs of the developers and
colleagues. The open source release is mainly to assist with our own management
of the code, as well as to allow others to use it. Bugfixes/suggestions are
nonetheless welcome. 


If you'd like to contribute any routines or fix bugs, consider forking the
code, inserting your changes and sending the maintainer a pull request.
Otherwise bug reports / suggestions can be sent to the maintainer Gareth
Davies, 'gareth.davies.ga.code@gmail.com' 

