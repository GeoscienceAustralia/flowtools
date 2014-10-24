#' LASextractor
#'
#' Codes for getting lidar data from las files into R, and some particular routines for
#' interactively extracting elevation transects along lines [e.g. ditches,
#' riverwalls, dry channel beds,etc] \cr
#'  \cr
#' You must have have the GPL programs las2txt and lasinfo (or las2txt.exe and
#' lasinfo.exe on windows) installed + in your search path \cr
#'
#' The main routines for users are (probably): \cr
#'  \cr
#'  readLas (read points from a las file); \cr
#'  \cr
#'  getLasFileExtent (get the bounding box of a .las file); \cr
#'  \cr
#'  getLasPointsNearRegion (get points from [multiple] las files in a
#' user-defined polygon, or within a set distance of a line);  \cr
#'  \cr
#'  lasPtsNearLine (get points from [multiple] las files near a line, and also
#' return information on the distance of each point along the line);  \cr
#'  \cr
#'  selectLasPts (interactively get subset of SpatialPointsDataFrame [output
#' from lasPtsNearLine] along a linear feature, based on a distance-elevation
#' curve plot); \cr
#'  \cr
#'  manuallyProjectLasElevationsAlongLine (combines the above 2 routines, can
#' be used to get elevation transects along linear features) \cr
#'  \cr
#'  gridXYZ (simple conversion of xyz to a raster, using an unstructured
#' interpolation routine) \cr
#' \cr
#' --\cr
#' The data in the examples was sourced from the OpenTopography Facility, with
#' acknowledgements below. The data in the package is a subset of the full
#' dataset: \cr
#' \cr
#' Dataset Name: Corpus Cristi, TX: Influence of Dunes & Barrier Islands on
#' Hurricane Surge(TX11_Ferreira), doi: 10.5069/G9862DC4, Survey Date 10/17/2011
#' \cr
#'  \cr
#' Dataset Acknowledgement: LiDAR data acquisition and processing completed by
#' the National Center for Airborne Laser Mapping (NCALM - http://www.ncalm.org).
#' NCALM funding provided by NSF's Division of Earth Sciences, Instrumentation and
#' Facilities Program. EAR-1043051.\cr
#'  \cr
#' Data Access Acknowledgement: This material is based on [data, processing]
#' services provided by the OpenTopography Facility with support from the National
#' Science Foundation under NSF Award Numbers 0930731 & 0930643 \cr
#'
#' @name LASextractor
#' @author Gareth Davies \email{gareth.davies.ga.code@@gmail.com}
#'
NULL

#' Convert points in las file to matrix using las2txt.
#'
#' The function uses a simple system call to las2txt, so requires that software
#' to be installed + in your search path
#' 
#' @param lasFile -- las filename
#' @param var -- flags for which variables we return, see las2txt help for
#'        info. The first 3 letters must be xyz
#' @param verbose -- TRUE/FALSE print lots of information
#' @param xRange -- length 2 numeric vector: Only extract points within these x values
#' @param yRange -- length 2 numeric vector: Only extract points within these y values
#' @param zRange -- length 2 numeric vector: Only extract points within these z values
#' @return Matrix with all var variables. These variables must be numeric
#' @export
#' @import rgdal
#' @import rgeos
#' @import unstructInterp
#' @examples
#'   lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#'   las_xyz=readLas(lasFile)
#'   summary(las_xyz)
#'   # Check the data size + basic properties
#'   stopifnot(all(dim(las_xyz)==c(744826,4)))
#'   stopifnot(round(mean(las_xyz[,1]))==680847)
#'   stopifnot(round(mean(las_xyz[,2]))==3062726)
readLas <- function(lasFile, var = "xyzc", verbose = FALSE, xRange = NULL, 
    yRange = NULL, zRange = NULL) {
    
    if (verbose) {
        print(paste0("Reading ", lasFile, " ....."))
    }
    
    extentFlag = ""
    if (!(is.null(xRange) | is.null(yRange))) {
        if (is.null(zRange)) {
            extentFlag = paste0(extentFlag, " --extent ", xRange[1], ",", yRange[1], 
                ",", xRange[2], ",", yRange[2])
        } else {
            extentFlag = paste0(extentFlag, " --extent ", xRange[1], ",", yRange[1], 
                ",", zRange[1], ",", xRange[2], ",", yRange[2], ",", zRange[2])
        }
    }
    
    # Choose the las2txt command in an OS-dependent way
    if (.Platform$OS.type == "unix") {
        las2TxtCommand = "las2txt"
    } else if (.Platform$OS.type == "windows") {
        las2TxtCommand = "las2txt.exe"
    } else {
        # Guess
        las2TxtCommand = "las2txt"
    }
    
    myCommand = paste0(las2TxtCommand, " --parse ", var, " --delimiter=\"|\" -i ", 
        lasFile, " -o stdout", extentFlag)
    outTxt = system(myCommand, intern = TRUE)
    if (!is.na(outTxt[1])) {
        myLas = matrix(as.numeric(unlist(strsplit(outTxt, split = "|", fixed = TRUE))), 
            ncol = nchar(var), nrow = length(outTxt), byrow = TRUE)
    } else {
        # No lidar points found [can happen if zRange excludes them all]
        myLas = c()
    }
    
    return(myLas)
}

#' Compute the bounding box of a lasFile using lasinfo
#'
#' @param lasFile - The las filename 
#' @return The bounding box of the lasFile
#' @export
#' @examples
#'   lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#'   lasExt = getLasFileExtent(lasFile)
#'   # Basic test of known dataset properties
#'   stopifnot(all(round(c(lasExt)) == c(680676, 3062544,  681076, 3062882)))
getLasFileExtent <- function(lasFile) {
    # Function to compute the lasFile extent Here we use the filenames Choose the
    # lasinfo command in an OS-dependent way
    if (.Platform$OS.type == "unix") {
        lasinfoCommand = "lasinfo"
    } else if (.Platform$OS.type == "windows") {
        lasinfoCommand = "lasinfo.exe"
    } else {
        # Guess
        lasinfoCommand = "lasinfo"
    }
    # Generic version
    lasinfoCMD = paste(lasinfoCommand, "--no-check", lasFile)
    outTxt = system(lasinfoCMD, intern = TRUE)
    MinXYZ = scan(text = gsub("Min X Y Z:", "", outTxt[grep("Min X Y Z", outTxt)]), 
        quiet = TRUE)
    MaxXYZ = scan(text = gsub("Max X Y Z:", "", outTxt[grep("Max X Y Z", outTxt)]), 
        quiet = TRUE)
    
    lasBaseXll = MinXYZ[1]
    lasBaseXur = MaxXYZ[1]
    lasBaseYll = MinXYZ[2]
    lasBaseYur = MaxXYZ[2]
    
    lasBbox = matrix(c(lasBaseXll, lasBaseXur, lasBaseYll, lasBaseYur), ncol = 2, 
        byrow = TRUE)
    rownames(lasBbox) = c("x", "y")
    colnames(lasBbox) = c("min", "max")
    
    return(lasBbox)
    
}

#' Compute the lasfile extent as a SpatialPolygons object
#'
#' @param lasFile - the las filename
#' @param CRSinfo - CRS object giving the coordinate system of the las data (and output polygon)
#' @return -- The extent of the lasFile as a SpatialPolygons object
#' @export 
#' @examples
#'   lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#'   # CRS of the data
#'   myCRS=CRS('+init=epsg:26914')
#'   # Get extent as a SpatialPolygons object
#'   mypol=getLasFileExtentPolygon(lasFile,myCRS)
#'   plot(mypol,axes=TRUE)
#'   emp = extent(mypol)
#'   # Basic test
#'   stopifnot(all(as.matrix(emp)==getLasFileExtent(lasFile)))
getLasFileExtentPolygon <- function(lasFile, CRSinfo) {
    # Function to compute the lasFile extent polygon
    lasExtent = getLasFileExtent(lasFile)
    p0 = Polygon(matrix(c(lasExtent[1, 1], lasExtent[1, 2], lasExtent[1, 2], lasExtent[1, 
        1], lasExtent[1, 1], lasExtent[2, 1], lasExtent[2, 1], lasExtent[2, 2], lasExtent[2, 
        2], lasExtent[2, 1]), ncol = 2), hole = FALSE)
    p1 = Polygons(list(p0), ID = "0")
    p2 = SpatialPolygons(list(p1), proj4string = CRSinfo)
    
    return(p2)
}

#'
#' Extract las points inside a given polygon
#'
#' @param myPol - A SpatialPolygons/SpatialPolygonsDataFrame object or 2-column
#'   matrix defining a polygon in which we want las points
#' @param lasFile - the las filename
#' @param xRange - length 2 vectors giving the min,max
#'  range of x to extract from the las file (before selecting points in
#'  the polygon). This can save time
#' @param yRange -- See xRange
#' @param zRange -- See xRange
#' @return - Matrix of las points
#' @export 
#' @examples
#'   lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#'   # CRS of the data
#'   myCRS=CRS('+init=epsg:26914')
#'   # Get extent as a SpatialPolygons object
#'   mypol=getLasFileExtentPolygon(lasFile,myCRS)
#'   # Now make a polygon by trimming off the outer 100m
#'   mypol_shrunk=gBuffer(mypol,width=-100)
#'   # Extract lidar points in mypol_shrunk
#'   point_subset=getLasFilePointsInPolygon(mypol_shrunk, lasFile)
#'   plot(mypol,axes=TRUE)
#'   plot(mypol_shrunk,add=TRUE,border='blue')
#'   points(point_subset[,1],point_subset[,2],col=point_subset[,3]+3,pch='.')
#'   title('Subset of the point cloud')
#'   # Check points are in the polygon
#'   p1 = mypol_shrunk@@polygons[[1]]@@Polygons[[1]]@@coords
#'   insiders=point.in.polygon( point_subset[,1],point_subset[,2],p1[,1],p1[,2])
#'   stopifnot(all(insiders==1))
#'
getLasFilePointsInPolygon <- function(myPol, lasFile, xRange = NULL, yRange = NULL, 
    zRange = NULL) {
    if (class(myPol) == "SpatialPolygons" | class(myPol) == "SpatialPolygonsDataFrame") {
        myPol = myPol@polygons[[1]]@Polygons[[1]]@coords
    }
    ld = readLas(lasFile, verbose = TRUE, xRange = xRange, 
                yRange = yRange, zRange = zRange)
    ldInMypol = point.in.polygon(ld[, 1], ld[, 2], myPol[, 1], myPol[, 2])
    nn = which(ldInMypol == 1)
    
    nearbyPoints = ld[nn, ]
    
    return(nearbyPoints)
}

#' Extract laspoints near a given polygon or line
#'
#' If bufWidth>0 then 'region' can be a line, which will be buffered to a
#' polygon. Alternatively 'region' can be a polygon, which supports any bufWidth
#' [likely bufWidth=0 is desired]
#'
#' @param region -- A SpatialLines or SpatialPolygons type object defining where we want las points
#' @param lasFiles -- A vector of las filenames to search through
#' @param bufWidth -- Numeric constant --  Buffer region by this width before
#'   extracting. Must be > 0 if region is a SpatialLines object.
#' @param zRange -- length 2 numeric vector -- only extract points within this elevation range
#' @return -- Matrix with las points
#' @export 
#' @examples
#' lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#' lineShpFile=paste0(system.file(package='LASextractor'),'/extdata/Linear_extraction_line.shp')
#' myLine=readOGR(dsn=lineShpFile,layer=gsub('.shp','',basename(lineShpFile)))
#' bufWidth=20.
#' lasSubset=getLasPointsNearRegion(myLine, lasFile, bufWidth=bufWidth)
#' plot(lasSubset[,1:2],col=lasSubset[,3]+2,asp=1, main='Points near the line',pch='.',cex=1.5)
#' plot(myLine,add=TRUE,col='black',lwd=3)
#'
#' # Test that the point subset is inside the buffered line
#' bufLine=gBuffer(myLine,width=bufWidth)
#' p1 = bufLine@@polygons[[1]]@@Polygons[[1]]@@coords
#' pts_in_bufLine = point.in.polygon( lasSubset[,1],lasSubset[,2],p1[,1],p1[,2])
#' stopifnot(all(pts_in_bufLine==1))
getLasPointsNearRegion <- function(region, lasFiles, bufWidth, zRange = NULL) {
    # Apply get_lasFilePoints_near_line efficiently over a vector of lasFiles
    
    CRSinfo = CRS(proj4string(region))
    nearbyPoints = c()
    
    myPolSp = gBuffer(region, width = bufWidth)
    myPol = coordinates(gBoundary(myPolSp))[[1]][[1]]
    
    for (lasFile in lasFiles) {
        lasExtent = getLasFileExtentPolygon(lasFile, CRSinfo)
        if (gIntersects(lasExtent, myPolSp)) {
            # Find the xy range of the intersection to reduce the number of points we rip out
            # of the las
            lineIntersection = gIntersection(lasExtent, myPolSp)
            xRange = bbox(lineIntersection)[1, ]
            xRange = c(floor(xRange[1]), ceiling(xRange[2]))
            yRange = bbox(lineIntersection)[2, ]
            yRange = c(floor(yRange[1]), ceiling(yRange[2]))
            nearbyPoints = rbind(nearbyPoints, getLasFilePointsInPolygon(myPol, 
                lasFile, xRange, yRange, zRange))
        }
        
    }
    
    return(nearbyPoints)
}

#' Convert a line to points + along-line-distance
#'
#' Given a SpatialLines object myLine, sample it at point locations and
#' compute their associated along-line distance.
#' All points originally on the line are included,
#' but segments are divided to ensure spacing < maxSpacing
#' 
#' @param myLine -- SpatialLines object
#' @param maxSpacing -- Maximum distance allowed between points on the output
#'        line. All points originally on the line will be included
#' @return -- Matrix with x,y,distance
#' @export 
#' @examples
#' lineShpFile=paste0(system.file(package='LASextractor'),
#'                    '/extdata/Linear_extraction_line.shp')
#' myLine=readOGR(dsn=lineShpFile,layer=gsub('.shp','',basename(lineShpFile)))
#' newLine=line2XyDist(myLine,maxSpacing=10)
#' plot(myLine,asp=1,lwd=3)
#' points(newLine[,1:2],col='red')
#' 
#' # Test that all origPts are on the line
#' origPts=myLine@@lines[[1]]@@Lines[[1]]@@coords
#' for(i in 1:length(origPts[,1])){
#'    s = abs(origPts[i,1]-newLine[,1]) + abs(origPts[i,2]-newLine[,2])
#'    stopifnot(min(s)==0)
#' }
#' # Test that the max spacing between points on newLine is 10
#' dl = diff(newLine[,1])**2 + diff(newLine[,2])**2
#' stopifnot(max(dl)<= 10**2)
#' # Check that distances are ok up to rounding error
#' stopifnot( max( abs( newLine[,3] - cumsum(c(0,sqrt(dl))) )) < 1.0e-08)
#'
line2XyDist <- function(myLine, maxSpacing = NULL) {
    
    myLineLength = SpatialLinesLengths(myLine)
    myLinePoints = coordinates(myLine)[[1]][[1]]
    # Distance between consecutive segments
    myLineSegmentDists = sqrt(diff(myLinePoints[, 1])^2 + diff(myLinePoints[, 2])^2)
    myLineCumDist = c(0, cumsum(myLineSegmentDists))
    
    LinePointsX = approxfun(myLineCumDist, myLinePoints[, 1], rule = 2)
    LinePointsY = approxfun(myLineCumDist, myLinePoints[, 2], rule = 2)
    
    # Compute new line-segment distances which are < maxSpacing apart
    if (!is.null(maxSpacing)) {
        newLineSegmentDists = c()
        for (i in 1:length(myLineSegmentDists)) {
            # Compute how many extra points we need on this segment to have
            # spacing<maxSpacing
            numExtraSegPts = floor(myLineSegmentDists[i]/maxSpacing)
            # Make the new segments
            newSpacing = myLineSegmentDists[i]/(numExtraSegPts + 1) * 
                         rep(1, (numExtraSegPts + 1))
            newLineSegmentDists = c(newLineSegmentDists, newSpacing)
        }
    } else {
        newLineSegmentDists = myLineSegmentDists
    }
    
    newLineDists = c(0, cumsum(newLineSegmentDists))
    newLinePoints = cbind(LinePointsX(newLineDists), LinePointsY(newLineDists), 
        newLineDists)
    
    return(newLinePoints)
}

#' Assign 'along-line' distances to a set of points based on a line
#' 
#' Suppose we have a line L and some points P which roughly follow L (but are
#' not all on it). For example consider the x,y coordinates of some lidar 
#' points which roughly follow a levee. We want to assign an
#' along-line distance to each point P, based on nearby points on L. \cr
#' The (basic) approach here is to sample a set of points P' along L (which are
#' separated by distance <= linePtSpacing),
#' and then compute the linear distances along L for each P'. Then for each
#' point in
#' P, we find the nearest point in P', and assign a linear distance to each P
#' point based on the linear
#' distance of the nearest P' point. \cr
#' Note this could fail if linePtSpacing is
#' not small enough, where 'small_enough' will be case dependent.
#'
#'
#' @param myLine -- SpatialLines object
#' @param nearbyPoints -- xyz.... matrix with 3 or more columns. First 3
#'   columns are xyz (defining the points P described above)
#' @param linePtSpacing -- Sample myLine with points separated by <=
#'   linePtSpacing (to make the P' points described above)
#' @return SpatialPointsDataFrame with coordinates = nearbyPoints[,1:2] and
#'   data = cbind(nearbyPoints[:,-c(1,2)], along_line_distance)
#' @export 
#'
#' @examples
#'     lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#'     lineShpFile=paste0(system.file(package='LASextractor'),'/extdata/Linear_extraction_line.shp')
#'     myLine=readOGR(dsn=lineShpFile,layer=gsub('.shp','',basename(lineShpFile)))
#'     # Get points near myLine
#'     lasSubset=getLasPointsNearRegion(myLine, lasFile, bufWidth=20.)
#'     # Assign distances along the line to the points [based on sub-sampling
#'     # myline at a max distance of 0.25m]
#'     lasSubset_dist=assignLinearDistancesToPoints(myLine,lasSubset,0.25)
#'     # Informative plot
#'     .plotOutPts(lasSubset_dist)
#'     # Test
#'     lp = line2XyDist(myLine, maxSpacing=0.25)
#'     clsd = coordinates(lasSubset_dist)
#'     nearest_lp=nearest_neighbour_interpolation(lp, 1:length(lp[,1]), clsd)
#'     dist_nearest_lasPt = sqrt((lp[nearest_lp,1]- clsd[,1])**2 +  (lp[nearest_lp,2]- clsd[,2])**2 )
#'     stopifnot(all(lasSubset_dist$ptDist == lp[nearest_lp,3]))
#'     # The nearest line point should be < (bufwidth + small increase for finite line point spacing)
#'     stopifnot(max(dist_nearest_lasPt) <= sqrt(20.^2 + (0.25/2)^2))
assignLinearDistancesToPoints <- function(myLine, nearbyPoints, linePtSpacing) {
    
    # Step 1: Break up line to points
    myLineAsPoints = line2XyDist(myLine, maxSpacing = linePtSpacing)
    # For each nearby_point, find nearest point in myLineAsPoints, and use for
    # interpolation
    ptDist = nearest_neighbour_interpolation(myLineAsPoints[, 1:2], myLineAsPoints[, 
        3], nearbyPoints[, 1:2])
    
    # Write out
    outPts = SpatialPointsDataFrame(
        nearbyPoints[, 1:2], 
        data = data.frame(cbind(nearbyPoints[, -c(1, 2)], ptDist)), 
        proj4string = CRS(proj4string(myLine)))
    return(outPts)
}

#' Get lasPts which are within bufWidth of a line, and assign to each a distance along the line
#'
#' This is a convenienve routine combining getLasPointsNearRegion and
#' assignLinearDistancesToPoints,
#' for the case that we want lasPoints near a line.
#' It also writes outputs to a file if desired.
#' It returns a SpatialPointsDataFrame with the nearby points + information on
#' their distance along the line
#'
#' @param lineFile -- a line shapefile
#' @param lasFiles -- a vector of las file names to search for points
#' @param bufWidth -- return points within this distance of line shapefile
#' @param outdir -- directory where output shapefile is written. Set this to NA
#'   to not write anything
#' @param zRange -- only take points in this elevation range. Ignored if NULL
#' @param lineDistanceRes -- To assign an along-line distance to the las
#'      points, the line is sampled as points with max spacing = lineDistanceRes. All
#'      the original line-vertices are retained. If this is too coarse, the
#'      along-line-distance computation could be inaccurate.
#' @return A SpatialPointsDataFrame with the chosen las points. The first 3
#'   columns will be xyz, and the final column will have the point distances along
#'   lineFile (as defined using 'assignLinearDistancesToPoints')
#' @export 
#' @examples
#' lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#' lineShpFile=paste0(system.file(package='LASextractor'),'/extdata/Linear_extraction_line.shp')
#' lasSubset=lasPtsNearLine(lineShpFile, lasFile, bufWidth=10., outdir=tempdir())
#' l = ncol(lasSubset)
#' par(mfrow=c(1,1))
#' plot(lasSubset[,l][[1]],lasSubset[,1][[1]],
#'      xlab='Distance (m)',ylab='Elevation (m)',pch='.')
lasPtsNearLine <- function(lineFile, lasFiles, bufWidth, outdir = "LASPTS", 
    zRange = NULL, lineDistanceRes = bufWidth/20) {
    
    myLine = readOGR(lineFile, layer = gsub(".shp", "", basename(lineFile)), 
                     verbose = FALSE)
    nearbyPoints = getLasPointsNearRegion(myLine, lasFiles, bufWidth, 
                                          zRange = zRange)
    
    ## Project onto line
    outPts = assignLinearDistancesToPoints(myLine, nearbyPoints, lineDistanceRes)
    
    if (!is.na(outdir)) {
        dir.create(outdir, showWarnings = FALSE)
        writeOGR(outPts, dsn = outdir, layer = gsub(".shp", "", basename(lineFile)), 
            driver = "ESRI Shapefile", overwrite_layer = TRUE)
    }
    
    .plotOutPts(outPts)
    
    return(outPts)
}

#' @export
#' @keywords internal
.plotOutPts <- function(outPts, distElevOnly = FALSE) {
    # 
    
    if (length(outPts@data[1, ]) > 2) {
        # We have a classifer -- use it to color points
        colz = t(col2rgb(rainbow(11)[outPts@data[, 2] + 1]))
    } else {
        colz = t(col2rgb("black"))
    }
    ptTransparency = 50  # Make points semi-transparent so clusters of points have more emphasis
    colz = rgb(colz[, 1], colz[, 2], colz[, 3], 
               alpha = ptTransparency, maxColorValue = 255)
    
    if (!distElevOnly) {
        par(mfrow = c(2, 2))
        plot(outPts, asp = 1, pch = ".", main = "Birds-eye view", col = colz)
        # Seems we can see wall type features in the following plots
        plot(coordinates(outPts)[, 1], outPts@data[, 1], xlab = "X coord", ylab = "Elev", 
            pch = ".", main = "X vs Elev", col = colz)
        plot(coordinates(outPts)[, 2], outPts@data[, 1], xlab = "Y coord", ylab = "Elev", 
            pch = ".", main = "Y vs Elev", col = colz)
        l = ncol(outPts@data)
        plot(outPts@data[, l], outPts@data[, 1], xlab = "Distance", ylab = "Elev", 
            pch = ".", main = "Distance vs Elevation", col = colz)
    } else {
        par(mfrow = c(1, 1))
        l = ncol(outPts@data)
        plot(outPts@data[, l], outPts@data[, 1], xlab = "Distance", ylab = "Elev", 
            pch = ".", main = "Distance vs Elevation (use this to digitize)", col = colz)
    }
}

#' @export
#' @keywords internal
.readUserInput <- function() {
    
    choice = readline(prompt = "Are you happy with these points? ( y / n / q )")
    
    if (choice == "y" | choice == "") {
        return("y")
    } else if (choice == "n") {
        return("n")
    } else if (choice == "q") {
        stop("You chose to quit")
    } else {
        print("Invalid Choice: must be one of y / n / q . Lets try again")
        choice = .readUserInput()
        return(choice)
    }
}

#' @export
#' @keywords internal
.interactivePointSelection <- function(outPts) {
    ## The user click's some points along the profile plot, and decides if they are ok
    ## or not
    
    .plotOutPts(outPts, distElevOnly = TRUE)
    print("Now click points along the profile that you want to keep \n (right click to finish)")
    
    keepPts = locator(type = "o", col = "red", pch = 19, cex = 0.5)
    
    # Ask the user if they are happy with these points
    choice = .readUserInput()
    
    if (choice == "n") {
        keepPts = .interactivePointSelection(outPts)
    } else {
        keepPts = cbind(keepPts$x, keepPts$y)
        return(keepPts)
    }
}

#' Interactively select a subset of las points
#' 
#' Interactively extract subset of spatialPointsDataFrame (output from
#' lasPtsNearLine) along a feature, using its distance-elevation plot 
#'
#' 
#' Suppose the input xyz points roughly follow a feature of interest called a
#' 'riverwall', which is distinguished from the surrounding topography by its
#' elevation [realistic examples: a riverwall, a narrow drainage channel, a
#' ditch, an elevated road, a bridge, or similar].
#' Some input xyz points are on the riverwall 
#' , while others are beside it, and the difference is obvious from
#' the point elevation. Distances of input xyz points along the riverwall are
#' assumed known (see lasPtsNearLine). \cr
#' We want to extract those input points which are along the riverwall. \cr
#' To achieve this the input points are plotted as along-wall-distance-vs-elevation, and
#' the user interactively adds a distance-elevation-curve to the plot.
#' Finally, we return all input xyz points with |elevation -
#' distance-elevation-curve| < verticalThreshold, and the distance elevation curve.
#'
#' @param shpFile -- [as output from lasPtsNearLine] a point shapefile
#'   filename, OR a SpatialPointsDataFrame
#'   object with the same information. Attribute table has 'elevation' in first
#'   column, and 'distance' in the last column 
#' @param verticalThreshold -- returned points will be within
#'   'verticalThreshold' elevation distance of the interactively defined
#'   distance-elevation function.
#' @param subplotLength -- Optionally the digitization occurs on a sequence
#'   of distance-elevation plots, where each plot covers a horizontal length of
#'   approximately subplotLength (physical distance in metres). This is needed to 
#'   digitize long features
#' @return A list containing 1): The SpatialPointsDataFrame containing points in
#'    shpFile that are within verticalThreshold of the clicked points; 
#'    2) The distance-elevation curve that the user clicked off
#' @export
#' @examples
#' \dontrun{
#'     lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#'     lineShpFile=paste0(system.file(package='LASextractor'),'/extdata/Linear_extraction_line.shp')
#'     lasSubset=lasPtsNearLine(lineShpFile, lasFile, bufWidth=10., outdir=tempdir())
#'     # Click off points on a plot, and return all points within 20cm of line defined by clicks
#'     newPts = selectLasPts(lasSubset,verticalThreshold=0.2)
#' }
selectLasPts <- function(shpFile, verticalThreshold, subplotLength=Inf) {
    
    if (class(shpFile) == "SpatialPointsDataFrame") {
        outPts = shpFile
    } else {
        outPts = readOGR(dsn = shpFile, layer = gsub(".shp", "", basename(shpFile)), 
            verbose = FALSE)
    }

    # Compute the along-feature span of the data
    l = ncol(outPts@data)
    minDist = min(outPts@data[,l])
    maxDist = max(outPts@data[,l])
    featureLength = maxDist - minDist
    if(subplotLength != Inf){
        # Break up the data into subsets to allow for multiple interactive
        # plots
        numberOfSubsets = ceiling(featureLength/subplotLength)
    }else if(subplotLength <=0){
        stop('subplotLength must be > 0')
    }else{
        # Trivial case
        numberOfSubsets = 1
    }
  
    # Loop over all the subplots and manually digitize 
    for(i in 1:numberOfSubsets){

        # Find the indices of points in this subplot
        lowerInterval = minDist + (i-1)*featureLength/numberOfSubsets
        upperInterval = minDist + i*featureLength/numberOfSubsets
        thisPlotSubset = which(outPts@data[,l]>=lowerInterval & outPts@data[,l]<=upperInterval)
        
        keepPts = .interactivePointSelection(outPts[thisPlotSubset])
        if(i==1){
            keepPtsGlobal = keepPts
        }else{
            # Append these keepPts to the last ones
            keepPtsGlobal = rbind(keepPtsGlobal, keepPts)
        }

    }

    # if(FALSE){ # Now for all outPts find the nearest keepPts [according to the
    # along-profile distance]
    # nearest_keepPts=nearest_neighbour_interpolation(keepPts, 1:length(keepPts[,1]),
    # outPts@data[,2:1]) keepers=which(
    # (abs(outPts@data[,1]-keepPts[nearest_keepPts,2])<verticalThreshold)&
    # (abs(outPts@data[,l]-keepPts[nearest_keepPts,1])<horizontal_threshold)) }else{
    # Use linear interpolation instead
    heightApproxFun = approxfun(keepPts[, 1], keepPts[, 2], rule = 2)
    keepers = which(abs(outPts@data[, 1] - heightApproxFun(outPts@data[, l])) < verticalThreshold)
    # }
    
    
    return(list(rawPts = outPts[keepers, ], distanceElevCurve = keepPts))
}


#' Extract elevations along linear features using las-points + input line
#' shapefiles + manual methods
#'
#' Given a line (read from shapefile 'extractionLineFile') and a set of las
#' files (filenames in vector 'lasFiles'): \cr
#' 1) Find all las points within 'extractionBufWidth' of the extraction line. \cr
#' 2) Compute upstream distances of the extracted points along projectionLineFile. \cr
#' 3) Manually select the lasPts we want to look at by digitising on a Distance-elevation plot,
#'    and accepting points within 'verticalThreshold' of our digitized line. \cr
#' 4) Write these out to outdir. They can be used to check visually which points were selected \cr
#' 5) Sample points along the line from 'projectionLineFile' (keeping all original points, 
#'    but adding new ones so that the spacing < 'linePtMaxSpacing'). 
#'    and use the manually digitised distance-elevation curve to assign an elevation to each of these. \cr
#' 6) Write it out as a csv. This can be used to define an xyz elevation profile. 
#'
#' @param lasFiles -- vector of names of lasFiles to search in
#' @param extractionLineFile -- Line shapefile. Initially extract all las
#'   points within 'extractionBufWidth' of 'extractionLineFile'
#' @param extractionBufWidth -- Extract points within this distance of
#'   extractionLineFile.
#' @param projectionLineFile -- Line shapefile. Project the extracted points
#'   onto this line for export. Often the same as extractionLineFile.
#' @param verticalThreshold -- keep las points within this vertical distance of
#'   the manually digitized distance-elevation line
#' @param outdir -- write outputs to this directory
#' @param linePtMaxSpacing -- Sample points with at most this spacing along
#'   projection_linefile before output
#' @param zRange -- Only extract las points within this elevation range
#'  (applies during the initial read of las files)
#' @param lineDistanceRes -- To assign an along-line distance to the las
#'      points, the line is sampled as points with max spacing = lineDistanceRes. All
#'      the original line-vertices are retained. If this is too coarse, the
#'      along-line-distance computation could be inaccurate.
#' @param subplotLength -- Distance in m. If the extractionLineFile is longer than this,
#'      then the digitization step will be split into a sequence of sub-plots, each
#       covering a length of line ~= subplotLength 
#' @return Nothing, but produces various output shapefiles and a csv
#' @export
#' @examples
#' \dontrun{
#'     # Cant auto-test this routine since it relies on user input
#'     lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#'     lineShpFile=paste0(system.file(package='LASextractor'),'/extdata/Linear_extraction_line.shp')
#'     # Manually select a subset, and write outputs as csv
#'     manuallyProjectLasElevationsAlongLine(
#'             lasFile, lineShpFile, extractionBufWidth=10., 
#'             projectionLineFile=lineShpFile, verticalThreshold=0.2, 
#'             outdir=tempdir(), linePtMaxSpacing=3,zRange=NULL)
#' }
manuallyProjectLasElevationsAlongLine <- function(lasFiles, 
    extractionLineFile, extractionBufWidth, projectionLineFile, 
    verticalThreshold, outdir, linePtMaxSpacing, zRange, 
    lineDistanceRes = extractionBufWidth/20,
    subplotLength = Inf) {
    
    # Get all lasPts near the line
    outPts = lasPtsNearLine(lineFile = extractionLineFile, lasFiles = lasFiles, 
        bufWidth = extractionBufWidth, outdir = outdir, zRange = zRange, 
        lineDistanceRes = lineDistanceRes)
    
    myLine = readOGR(projectionLineFile, 
        layer = gsub(".shp", "", basename(projectionLineFile)), 
        verbose = FALSE)
    
    if (projectionLineFile != extractionLineFile) {
        # Compute distances of extracted points along projectionLineFile Prior to this
        # the distances are computed along extractionLineFile
        outPts = cbind(coordinates(outPts), as.matrix(outPts@data))  # Need matrix
        outPts = assignLinearDistancesToPoints(myLine, outPts, lineDistanceRes)
    }
    
    # Manually filter them
    filteredOutPts = selectLasPts(outPts, verticalThreshold = verticalThreshold)
    # Write out filtered points
    filtered_points_basename = paste0(gsub(".shp", "", basename(extractionLineFile)), 
        "_filtered")
    
    writeOGR(filteredOutPts$rawPts, dsn = outdir, layer = filtered_points_basename, 
        driver = "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Now read the line, convert it to points with max spacing linePtMaxSpacing,
    # extract points, and interpolate elevations onto them from the
    # filteredOutPts$distanceElevCurve
    myLineAsPoints = line2XyDist(myLine, maxSpacing = linePtMaxSpacing)
    myElevFun = approxfun(filteredOutPts$distanceElevCurve[, 1], 
        filteredOutPts$distanceElevCurve[, 2], rule = 2)
    myLineElevs = myElevFun(myLineAsPoints[, 3])
    
    # Write out
    linearPointsBasename = paste0(gsub(".shp", "", basename(projectionLineFile)), 
        "_linearPts.csv")
    write.table(cbind(myLineAsPoints[, 1:2], myLineElevs), file = paste0(outdir, 
        "/", linearPointsBasename), sep = ",", row.names = F, col.names = F)
}



#' Convert xyz data to raster
#'
#' Grid XYZ data and produce a raster with extent equal to the XYZ data extent
#'
#' If CRSinfo = NULL then the raster is not georeferenced, but the projection
#' can be specified later
#' 
#' @import raster
#' @param myXYZ -- 3 column matrix with XYZ data
#' @param dx -- pixel x size for new raster
#' @param dy -- pixel y size for new raster
#' @param CRSinfo -- the projection information for the new raster as a CRS
#'   object. If NULL, no projection is defined (but this can be set later)
#' @param interpMethod -- the interpolation method to use. Supported methods
#'   are 'triangular_interpolation' and 'nearest_neighbour_interpolation'
#' @return raster object with the gridded data
#' @export
#' @examples
#'   lasFile=paste0(system.file(package='LASextractor'), '/extdata/points.las')
#'   # CRS of the data
#'   myCRS=CRS('+init=epsg:26914')
#'   # Get extent as a SpatialPolygons object
#'   mypol=getLasFileExtentPolygon(lasFile,myCRS)
#'   # Now make a polygon by trimming off the outer 100m
#'   mypol_shrunk=gBuffer(mypol,width=-100)
#'   # Extract lidar points in mypol_shrunk
#'   point_subset=getLasFilePointsInPolygon(mypol_shrunk, lasFile)
#'   # Make a raster and plot it
#'   point_subset_rast=gridXYZ(point_subset, dx=0.5,dy=0.5, CRSinfo=myCRS,
#'                             interpMethod='triangular_interpolation')
#'   plot(point_subset_rast)
#'   # Bit of testing
#'   point_subset_rast=gridXYZ(point_subset, dx=0.5,dy=0.5, CRSinfo=myCRS,
#'                             interpMethod='nearest_neighbour_interpolation')
#'   # Convert back to points, then find nearest_neighbours in point_subset,
#'   # and check it is correct
#'   point_subset_rastPts = rasterToPoints(point_subset_rast)
#'   nearest_lasPts = nearest_neighbour_interpolation(point_subset[,1:2],
#'       1:length(point_subset[,1]), point_subset_rastPts[,1:2])
#'   stopifnot(all(point_subset_rastPts[,3]==point_subset[nearest_lasPts,3]))
gridXYZ <- function(myXYZ, dx = 1, dy = 1, CRSinfo = NULL, 
                    interpMethod = "triangular_interpolation") {
    # New gridding function with other interpolations
    xnew = seq(min(myXYZ[, 1]), max(myXYZ[, 1]), by = dx)
    ynew = seq(max(myXYZ[, 2]), min(myXYZ[, 2]), by = -dy)
    newGrid = as.matrix(expand.grid(xnew, ynew))
    
    if (interpMethod == "triangular_interpolation") {
        gridElev = triangular_interpolation(as.matrix(myXYZ[, 1:2]), myXYZ[, 3], 
            newGrid)
    } else if (interpMethod == "nearest_neighbour_interpolation") {
        gridElev = nearest_neighbour_interpolation(as.matrix(myXYZ[, 1:2]), myXYZ[, 
            3], newGrid)
    } else {
        stop("interpMethod not recognized")
    }
    gridElev = matrix(gridElev, ncol = length(xnew), nrow = length(ynew), byrow = TRUE)
    # library(raster)
    newRast = raster::raster(gridElev, xmn = min(xnew) - dx/2, ymn = min(ynew) - 
        dy/2, xmx = max(xnew) + dx/2, ymx = max(ynew) + dy/2)
    
    if (!is.null(CRSinfo)) {
        proj4string(newRast) = CRSinfo
    }
    return(newRast)
} 
