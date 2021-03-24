import matplotlib
matplotlib.use('Agg')
import pylab as pl
import matplotlib.pyplot as pplot
from shapely.geometry import mapping, Polygon, LineString, LinearRing, Point
import fiona
import netCDF4
import datetime
import time
import numpy as np
import collections
import simplekml
import math
import sys
from optparse import OptionParser
import os
import atexit
import argparse
import subprocess
import math
import tempfile
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Notes:
# 
# Extracting data from the input file (ADCIRC output file in .nc format) 
# as numpy arrays.
# Longitude (lon) and Latitude (lat) of every vertex in the entire domain.
# Description of triangular elements (nv) as numpy arrays of 3 vertices 
# for each triangle.
# ADCIRC output values (like maximum water elevation, maximum wave 
# height, maximum wind speed etc.) stored against the corresponding 
# variable name (var)
#
# If the ADCIRC output file contains `_`FILL VALUE (= -99999) var gets 
# stored as a masked numpy array. The masked or redundant value is 
# replaced by -100.
# 
# For better visualization the extreme values for each variable 
# are readjusted.
#
# Time step information (time`_`var) Used to extract startdate.
#
# The time step values are converted into 'datetime' objects and then 
# into a string format to use while writing shapefiles later 
# (used for time series output data).
#
# matplotlib.use('Agg') is used to specify a matplotlib backend 
# which if not specified creates an error while running in cluster 
#
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#     P A R S E   C O M M A N D   L I N E   O P T I O N S
#---------------------------------------------------------------------
parser = OptionParser()
parser.add_option("-t", "--units",dest="units", default="si", help="english or si units")
parser.add_option("-d", "--datumlabel",dest="datumlabel", default="MSL", help="label for units in color scale, e.g., 'NAVD88' or 'local ground level'")
parser.add_option("-n", "--storm", dest="storm", default="null",help="name of storm for labelling plot")
parser.add_option("-m", "--filename", dest="filename", default="null", help="name of file to read")
parser.add_option("-f", "--filetype", dest="filetype", default="maxele.63.nc", help="type of file to read")
parser.add_option("-s", "--polytype", dest="polytype", default="polygon", help="polyline or polygon")
parser.add_option("-v", "--viztype",dest="viztype", default="kmz", help="kmz or shapefile")
parser.add_option("-p", "--palettename", dest="palettename", default="water-level.pal", help="palette file name for the contour color scale")
parser.add_option("-o", "--outputfile", dest="outputfile", default='null', help="name of output file")
parser.add_option("-g", "--logofile", dest="logofile", default='logo.png', help="name of logo image file")
parser.add_option("-i", "--logodims", dest="logodims", default='null', help="dimensions of logo image file")
parser.add_option("-q", "--logounits", dest="logounits", default='fraction', help="units of logo image file")
parser.add_option("-b", "--subplots", dest="subplots", default="no", help="whether to make subplots first (yes or no)")
parser.add_option("-e", "--contourlevels", dest="contourlevels", default="null", help="contour levels")
parser.add_option("-r", "--contourrange", dest="contourrange", default="null", help="contour range min max increment, e.g., '-10 10 1 for contours every 1 unit from -10 to +10")
parser.add_option("-k", "--ticks", dest="specifiedticks", default="null", help="colorbar tick labels")
parser.add_option("-l", "--lonlatbox", dest="l", default="36 33.5 -60 -100",help="local box : NorthLat SouthLat EastLong WestLong")
parser.add_option("-u", "--lonlatbuffer", dest="lonlatbuffer", default="0",help="longitude latitude buffer")
parser.add_option("-c", "--datumconv", dest="datumconv", default="no", help="datum conversion from MSL to NAVD88 (yes or no)")
parser.add_option("-x", "--datumtextfile", dest="datumtextfile", default="rasterdeltas_capped.txt", help="name of text file for datum conversion")
parser.add_option("--grow", dest="grow", default="no", help="whether or not r.grow should be used (yes or no)")
parser.add_option("--growmethod", dest="growmethod", default="without", help="choose grow method: with (with subtraction method) or without")
parser.add_option("--createlocation", dest="createlocation", default="no", help="create a GRASS Location for use with r.grow extrapolation? (yes or no)")
parser.add_option("--raster", dest="rastername", help="file name of raster(s) entered as a string using the format specified in http://....")#Enter website information path to Kalpana help
parser.add_option("--resolution", dest="resolution", default=50, help="specify resolution in feet or accept current DEM resolution by entering 'align' (default=50ft.)")
parser.add_option("--epsg", dest="myepsg", default="null", help="enter the desired state plane EPSG code for your location. EPSG codes can be found using the following link: http://spatialreference.org/ref/?page=7&search=")
parser.add_option("--grownoutput", dest="grownoutput", default="WaterLevels_grown", help="enter a name for the final grown shapefile output")
parser.add_option("--createmethod", dest="createmethod", default="null", help="enter 'existing' if the GRASS location should be created using the DEM's existing datum")
parser.add_option("--growradius", dest="growradius", default=30.01, help="enter the maximum number of cells for r.grow extrapolation")
parser.add_option("--flooddepth", dest="flooddepth", default="no", help="display flood depth output [yes or no]")
parser.add_option("--grownfiletype", dest="grownfiletype", default="ESRI_Shapefile", help="filetype for grow and/or flooddepth output. Select from available OGR formats with GRASS GIS.")
(options, args) = parser.parse_args()
#nc=netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/10/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/maxele.63.nc').variables
#'http://opendap.renci.org:1935/thredds/dodsC/tc/arthur/12/nc6b/hatteras.renci.org/nclo/nhcConsensus/maxele.63.nc'
#
# time0 : the current time denoting the time when program run starts 
time0=time.time()
#
#When --createlocation is set to yes, a GRASS Location is created for use with r.grow (--grow option) and then Kalpana exits. A location named GRASS_LOCATION is created based on the --epsg input and temporary locations are created for each DEM input specified by the --raster input. All DEMs are projected into GRASS_LOCATION and now a file called GRASS_LOCATION.zip exists which contains all of the DEMs patched into one larger DEM and uses a correct stateplane datum, specified by the --epsg input.
if options.createlocation == "yes":
    myepsg=options.myepsg
    rastername=options.rastername
    resolution=options.resolution
    createmethod=options.createmethod
    if ',' in rastername:
	rastername = rastername.split(',') #If multiple input rasters exist, the input string is split into a list.
    else:
	rasternamestr = rastername
	rastername = []
	rastername.append(rasternamestr) #If one input raster exists, this is added to a list of length=1.

    #Working with GRASS without starting it explicitly; using metadata only.
    #This works assuming a linux operating system. If using a different operating system, see the website below.
    #More information: https://grasswiki.osgeo.org/wiki/Working_with_GRASS_without_starting_it_explicitly
    grass7bin = 'grass72'
    startcmd = grass7bin + ' --config path'
    p = subprocess.Popen(startcmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    if p.returncode != 0:
	    print >>sys.stderr, 'ERROR: %s' % err
	    print >>sys.stderr, "ERROR: Cannot find GRASS GIS 7 start script (%s)" % startcmd
	    sys.exit(-1)
    gisbase = out.strip('\n')
    os.environ['GISBASE'] = gisbase
    gpydir = os.path.join(gisbase, "etc", "python")
    appendir = sys.path.append(gpydir)
    gisdb = os.path.join(os.getenv('HOME','grassdata'))
    gisdb = os.path.join(tempfile.gettempdir(), 'grassdata')
    location = 'GRASS_LOCATION'#Specify the name of the location which is being created
    mapset = 'PERMANENT'
    cwd = os.getcwd()
    location_path = os.path.join(cwd,location)
    if createmethod == 'existing':
	startcmd = grass7bin + ' -c ' + rastername[0] + ' -e ' + location_path #Used for creating a location based on a raster
    else:
	startcmd = grass7bin + ' -c epsg:' + myepsg + ' -e ' + location_path #Used for creating a location based on an EPSG cod
    p = subprocess.Popen(startcmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    if p.returncode != 0:
	print >>sys.stderr, 'ERROR: %s' % err
	print >>sys.stderr, 'ERROR: Cannot generate location (%s)' % startcmd
	sys.exit(-1)
    else:
	print 'Created location %s' % location_path

    os.environ['GISDBASE'] = gisdb
    path = os.getenv('LD_LIBRARY_PATH')
    dir = os.path.join(gisdb, 'lib')
    if path:
	path = dir + os.pathsep + path
    else:
	path = dir
    os.environ['LD_LIBRARY_PATH'] = path
    os.environ['LANG'] = 'en_US'
    os.environ['LOCALE'] = 'C'

    #GRASS is now ready to be imported and used
    import grass.script as grass
    import grass.script.setup as gsetup

    gsetup.init(gisbase, cwd, location, mapset) #Setting the GRASS working environment

    #For multiple DEM inputs:
    if len(rastername) > 1:
	outList = []
	if resolution != 'align':
       	    for s in range(len(rastername)):#Iterating over each DEM
		#Import each DEM into GRASS_LOCATION
		grass.run_command('r.import',
		    overwrite=True,
		    input=rastername[s],
		    output='ras'+str(s),
		    resolution='value',
		    resolution_value=resolution,
		    resample='bilinear',
		    extent='input')
		outList.append('ras'+str(s))#Create a list of rasters in GRASS_LOCATION to be patched together
	else:
	    for s in range(len(rastername)):#Iterating over each DEM
		#Import each DEM into GRASS_LOCATION
		grass.run_command('r.import',
		    overwrite=True,
		    input=rastername[s],
		    output='ras'+str(s))
		rastInfo=grass.parse_command('r.info',map='ras'+str(s),flags='g',delimiter='=')
		nsres=rastInfo['nsres']
		ewres=rastInfo['ewres']
		print "North/South resolution of raster #{0}: {1}".format(s,nsres)
		print "East/West resolution of raster #{0}: {1}".format(s,ewres)
		outList.append('ras'+str(s))#Create a list of rasters in GRASS_LOCATION to be patched together
	    print "Warning: if DEM raster resolutions do not match, the aggregate DEM resolution will match the resolution of the first input raster (raster #0 above)."
	#Set the computational region of GRASS_LOCATION based on the extents of the DEMs
	grass.run_command('g.region',
	    raster=outList,
	    quiet=True)
	#Patch all dems into one large DEM named 'dem'
	grass.run_command('r.patch',
	    input=outList,
	    output='dem',
	    overwrite=True,
	    quiet=True)
	#Remove each individual smaller DEM from GRASS_LOCATION
	grass.run_command('g.remove',
	    type='raster',
	    name=outList,
	    flags='f',
	    quiet=True)
    #For one DEM input:
    elif len(rastername) == 1:
	#For a specified or default resolution:
	if resolution != 'align':
	    grass.run_command('r.import',
		overwrite=True,
		input=rastername[0],
		output='dem',
		resolution='value',
		resolution_value=resolution,
		resample='bilinear',
		extent='input')
	else:
	    #Import and automatically reproject the raster
	    grass.run_command('r.import',
		overwrite=True,
		input=str(rastername[0]),
		output='dem',
		extent='input',
		resolution='estimated')
	#Set the resolution and computational region to align with the DEM
	grass.run_command('g.region',
	    raster='dem')

    else:
	print "Please enter the raster name using the '--raster' option."
    os.system('zip -r GRASS_LOCATION.zip GRASS_LOCATION')#Zip GRASS_LOCATION
    os.system('rm -fr GRASS_LOCATION')#Remove unzipped GRASS_LOCATION folder
    print "Finished creating GRASS Location after %d seconds. GRASS_LOCATION.zip is now ready for use with r.grow extrapolation." % (time.time()-time0)
    sys.exit(0)#Exits after --createlocation finishes creating GRASS_LOCATION

# jgf: If there are no command line arguments, trigger the menu to 
# collect input parameters interactively. Otherwise, use the command 
# line arguments.
if options.storm == "null" : 
    print (30 * '-')
    print ("   M A I N - M E N U")
    print (30 * '-')
    print ("bathytopo (reads maxele.63.nc)")
    print ("maxele.63.nc")
    print ("maxwvel.63.nc")
    print ("swan_HS_max.63.nc")
    print ("fort.63.nc")
    print ("fort.74.nc")
    print ("swan_HS.63.nc")
    print ("swan_TPS_max.63.nc")
    print ("swan_TPS.63.nc")
    print ("swan_TMM10.63.nc")
    print (30 * '-')
    #
    # ACCEPTING USER INPUT
    storm = raw_input('Enter name of storm :')
    filetype = raw_input('Enter your choice of ADCIRC output to be visualized : ')
    polytype = raw_input('Enter your choice of vector shape (polyline or polygon) : ')
    viztype = raw_input('Enter your choice of visualization (GIS Shape File or Google Earth (KMZ))  (shapefile or kmz): ')
    subplots = raw_input('Do you want to make subplots first (with longlat boxes) and then plot full domain (yes or no)')
    if filetype == 'maxele.63.nc' or filetype == '2':
        datumconv = raw_input('Do you want to convert datum from MSL to NAVD-88 (yes or no): ')
        if datumconv == 'yes':
            datumtextfile = raw_input('Please specify name of raster text file for interpolation: ')
    if viztype == 'kmz' or viztype == 'Y':
        l= raw_input('Enter local box : NorthLat SouthLat EastLong WestLong :')
        if filetype == '1' or filetype == '2' or filetype == '3' or filetype == '4' or filetype == '8':
            lonlatbuffer = float(raw_input('enter long/lat buffer: '))
        elif filetype == '5' or filetype == '6' or filetype == '7':
            print 'KMZ files cannot be created for this file type'
    filename = 'null'
    logofile = 'logo.png'
    logodims = 'null'
    logounits = 'fraction'
    palettename = 'null'
    units = 'si'
    contourlevels = 'null'
    contourrange = 'null'
    datumlabel = 'msl'
    specifiedticks = 'null'
    outputfile = 'null'
    grow = 'no'
    growmethod = 'without'
    flooddepth = 'no'
    grownfiletype = 'ESRI_Shapefile'
else:
    filetype=options.filetype
    filename=options.filename
    polytype=options.polytype 
    viztype=options.viztype
    l=options.l
    subplots=options.subplots
    lonlatbuffer=float(options.lonlatbuffer)
    storm=options.storm      
    palettename=options.palettename
    outputfile=options.outputfile
    logodims = options.logodims
    logounits = options.logounits
    logofile=options.logofile    
    contourlevels=options.contourlevels
    contourrange=options.contourrange
    specifiedticks=options.specifiedticks
    units=options.units
    datumlabel=options.datumlabel
    datumconv=options.datumconv
    datumtextfile=options.datumtextfile
    grow=options.grow
    growmethod=options.growmethod
    grownoutput=options.grownoutput
    flooddepth=options.flooddepth
    grownfiletype=options.grownfiletype

if grow == "yes":
    polytype = "polygon"
    viztype = "shapefile"
    outputfile = "kalpana_out"

# 
# jgf: Change the input values to something more intuitive if necessary
#print 'INFO: storm is ' + storm
legacyPolyTypeMapping = { 'A' : 'polyline', 'B' : 'polygon' }
if polytype in legacyPolyTypeMapping.keys():
    polytype = legacyPolyTypeMapping[polytype]
#print 'INFO: polytype is ' + polytype
legacyVizTypeMapping = { 'X' : 'shapefile', 'Y' : 'kmz' }
if viztype in legacyVizTypeMapping.keys():
    viztype = legacyVizTypeMapping[viztype]
#print 'INFO: viztype is ' + viztype    
legacySubplotsMapping = { 'Y' : 'yes', 'N' : 'no' }
if subplots in legacySubplotsMapping.keys():
    subplots = legacySubplotsMapping[subplots]
#print 'INFO: subplots is ' + subplots
legacyFileTypeMapping = { '1' : 'bathytopo', '2' : 'maxele.63.nc', '3' : 'maxwvel.63.nc', '4' : 'swan_HS_max.63.nc', '5' : 'fort.63.nc', '6' : 'fort.74.nc', '7' : 'swan_HS.63.nc', '8' : 'swan_TPS_max.63.nc', '9' : 'swan_TPS.63.nc', '10' : 'swan_TMM10.63.nc' }
if filetype in legacyFileTypeMapping.keys():
    filetype = legacyFileTypeMapping[filetype]
#print 'INFO: filetype is ' + filetype
#
# jgf: designate certain files as time varying 
timeVaryingFiles = ['fort.63.nc', 'fort.74.nc', 'swan_HS.63.nc', 'swan_TPS_63.nc', 'swan_TMM10.63.nc']
if filetype in timeVaryingFiles: print "INFO: This file is time varying."
#
# jgf: Check for conflicts between input parameters. 
if viztype == 'kmz' and filetype in timeVaryingFiles: 
    print "ERROR: Cannot produce Google Earth (kmz) output for time varying files."
    exit
#
# DEFINING BINS FOR KML CREATION 
if viztype ==  'kmz':
    gdomain = [50,5,-60,-100]
    local = map(float,l.split())
    bins = []
    bins.append([local[1],gdomain[1],gdomain[2],gdomain[3]])
    north = local[1] + 0.5  
    south = local[1]
    while (north <= local[0]):       
        bins.append([north,south,gdomain[2],gdomain[3]])
        south = north
        north = north +0.5             
    bins.append([gdomain[0],local[0],gdomain[2],gdomain[3]])
    #print bins
#
fileTypesColorBarNames = { 'maxele.63.nc' : 'Colorbar-water-levels.png', 'swan_HS_max.63.nc' : 'Colorbar-wave-heights.png', 'maxwvel.63.nc' : 'Colorbar-wind-speeds.png', 'swan_TPS_max.63.nc' : 'Colorbar-wave-periods.png', 'bathytopo' : 'Colorbar-bathymetry.png' }
fileTypesNetCDFVarNames = { 'bathytopo' : 'depth', 'maxele.63.nc' : 'zeta_max', 'maxwvel.63.nc' : 'wind_max', 'swan_HS_max.63.nc' : 'swan_HS_max', 'fort.63.nc' : 'zeta', 'fort.74.nc' : [ 'windx', 'windy' ], 'swan_HS.63.nc' : 'swan_HS', 'swan_TPS_max.63.nc' : 'swan_TPS_max', 'swan_TPS.63.nc' : 'swan_TPS', 'swan_TMM10.63.nc' : 'swan_TMM10' }#######Step can be surpassed in sample maxele if zeta_max is changed to maxele after key 'maxele.63.nc'
fileTypesDefaultPaletteFileNames = { 'bathytopo' : 'mesh-bathy.pal', 'maxele.63.nc' : 'water-level.pal', 'maxwvel.63.nc' : 'wind-speed.pal', 'swan_HS_max.63.nc' : 'wavht.pal', 'swan_TPS_max.63.nc' : 'wavht.pal' }       
fileTypesKMLFolderNames = { 'bathytopo' : 'Bathymetry', 'maxele.63.nc' : 'Maximum-Water-Levels', 'maxwvel.63.nc' : 'Maximum Wind Velocity', 'swan_HS_max.63.nc' : 'Maximum-Wave-Heights', 'swan_TPS_max.63.nc' : 'Maximum-Wave-Period' }
fileTypesDefaultOutputShapeFileNames = { 'bathytopo' : 'mesh-bathy', 'maxele.63.nc' : 'water-level', 'maxwvel.63.nc' : 'wind-speed','swan_HS_max.63.nc' : 'wave-height', 'fort.63.nc' : 'nodalelev'+'_'+ polytype + '_'+ str(storm), 'fort.74.nc' : 'nodalwvel'+'_'+ polytype + '_'+ str(storm), 'swan_HS.63.nc' : 'nodalwavht'+'_'+ polytype + '_'+ str(storm), 'swan_TPS_max.63.nc' : 'wave-period', 'swan_TPS.63.nc' :  'nodalpeakpd'+'_'+polytype+'_'+str(storm), 'swan_TMM10.63.nc' : 'avgpd'+'_'+polytype+'_'+str(storm) }    
#
# use the default file name if the file name was not provided
if filename == 'null': 
   filename = filetype
#
# use the default palette file unless a different palette file was requested
if palettename == 'null':
   palettename = fileTypesDefaultPaletteFileNames[filetype]
#
if units == 'english':
    fileTypesDefaultContourLevels = { 'bathytopo' : [-40,-30,-20,-10,-5,0,2,4,6,8,10,15,20,30,40,50,100,200,300,400,500,550], 'maxele.63.nc' : [0,1,2,3,4,5,6,7,8], 'maxwvel.63.nc' : [0,10,20,30,40,50,60,70,80,90], 'swan_HS_max.63.nc' : [0,4,8,12,16,20,24,28,32], 'fort.63.nc' : [-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5], 'fort.74.nc' : [0,10,20,30,40,50,60,70,80,90], 'swan_HS.63.nc' : [0,4,8,12,16,20,24,28,32], 'swan_TPS_max.63.nc' : [ 1, 2, 3, 4, 5, 6, 8, 10 ], 'swan_TPS.63.nc' : [ 1, 2, 3, 4, 5, 6, 8, 10 ], 'swan_TMM10.63.nc' : [ 1, 2, 3, 4, 5, 6, 8, 10 ] }
else:
    # default contour range for si units
    fileTypesDefaultContourLevels = { 'bathytopo' : [-40,-30,-20,-10,-5,0,2,4,6,8,10,15,20,30,40,50,100,200,300,400,500,550], 'maxele.63.nc' : [0,0.25,0.5,0.75,1,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25], 'maxwvel.63.nc' : [0,5,10,15,20,25,30,35,40,45], 'swan_HS_max.63.nc' : [0,1,2,3,4,5,6,7,8], 'fort.63.nc' : [-3, -2.5, -2.0, -1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0], 'fort.74.nc' : [0,5,10,15,20,25,30,35,40,45], 'swan_HS.63.nc' : [0,1, 2, 3, 4, 5, 6, 7, 8], 'swan_TPS_max.63.nc' : [ 1, 2, 3, 4, 5, 6, 8, 10 ], 'swan_TPS.63.nc' : [ 1, 2, 3, 4, 5, 6, 8, 10 ], 'swan_TMM10.63.nc' : [ 1, 2, 3, 4, 5, 6, 8, 10 ] }
#
# ROUTINE TO CALCULATE CONTOUR LEVELS 
def generateContourLevelsFromMinMaxAndIncrement(minl,maxl,inc):
    contourLevels = []
    clevel = minl
    while clevel <= maxl:
        contourLevels.append(clevel)
        clevel = clevel + inc
    return contourLevels
#
# jgf: Use the default contour levels unless a different set of contour levels
# was requested, or a contour range was specified. If both a range
# and specific levels were specified, the levels take precedence.
levels = []
if contourlevels == 'null':
    if contourrange == 'null':
        levels = fileTypesDefaultContourLevels[filetype]
    else:
        # use the specified range and increment
        rangelimits = contourrange.split()
        levels = generateContourLevelsFromMinMaxAndIncrement(float(rangelimits[0]),float(rangelimits[1]),float(rangelimits[2]))
else:
    # use the specified contour levels from the command line
    for clevel in contourlevels.split():
        levels.append(float(clevel))
#
# jgf: Use the tick values specified by the analyst, if any; otherwise
# just use the contour levels
ticks = []
if specifiedticks == 'null':
    ticks = levels
else:
    for ctick in specifiedticks.split():
        ticks.append(float(ctick))
#
# Read the data
nc = netCDF4.Dataset(filename).variables
if filetype not in timeVaryingFiles: 
    var = nc[fileTypesNetCDFVarNames[filetype]][:]
#
# jgf: designate certain files as needing to have -99999 values filled in
requireFill = ['maxele.63.nc', 'swan_HS_max.63.nc', 'fort.63.nc', 'swan_HS.63.nc', 'swan_TPS_max.63.nc', 'swan_TPS.63.nc', 'swan_TMM10.63.nc']
#
# jgf: fill -99999 values with -100 values to avoid using a masked array
# because kmz generator cannot handle masked arrays
if viztype == 'kmz' and filetype in requireFill and filetype not in timeVaryingFiles:
    var = var.filled(-100)
#
# jgf: specify conversion factors for conversion to english units
# (m to ft), (m/s to mph)
fileTypesEnglishUnitsConversions = { 'bathytopo' : 3.2808399, 'maxele.63.nc' : 3.2808399, 'maxwvel.63.nc' : 2.2369363, 'swan_HS_max.63.nc' : 3.2808399, 'fort.63.nc' : 3.2808399, 'fort.74.nc' : 2.2369363, 'swan_HS.63.nc' : 3.2808399, 'swan_TPS_max.63.nc' : 1.0, 'swan_TPS.63.nc' : 1.0, 'swan_TMM10.63.nc' : 1.0 }
#
# jgf: Convert units if english units were specified
if units == 'english':
    if filetype not in timeVaryingFiles : 
        var = np.multiply(float(fileTypesEnglishUnitsConversions[filetype]),var)
    fileTypesUnitLabels = { 'maxele.63.nc' : 'Water Level (feet above ' + datumlabel + ')', 'fort.63.nc' : 'Water Level (feet above ' + datumlabel + ')', 'swan_HS_max.63.nc' : 'Wave Height (feet)', 'maxwvel.63.nc' : 'Wind Speed at Ground Level (mph)', 'swan_TPS_max.63.nc' : 'Peak Wave Period (s)', 'bathytopo' : 'Bathymetry (ft below ' + datumlabel + ')'}
else:
    fileTypesUnitLabels = { 'maxele.63.nc' : 'Water Level (m above ' + datumlabel + ')', 'fort.63.nc' : 'Water Level (m above ' + datumlabel + ')', 'swan_HS_max.63.nc' : 'Wave Height (m)', 'maxwvel.63.nc' : 'Wind Speed at Ground Level (m/s)', 'swan_TPS_max.63.nc' : 'Peak Wave Period (s)', 'bathytopo' : 'Bathymetry (m below ' + datumlabel +')'}
#
# jgf: limit data values based on the contouring range for kmz
if filetype not in timeVaryingFiles:
    np.place(var,var > max(levels),max(levels)-0.01)   
    np.place(var,(-100 < var) & (var < min(levels)),min(levels)) 
#
timestep = 'time'
#
# Extracting longitude and latitude of all mesh nodes 
lon = nc['x'][:]
lat = nc['y'][:]
#
# Extracting element description details (list of 3 vertices specified 
# for each triangle denoted by index number) from the output file 
nv = nc['element'][:,:] -1
#
# read datum conversion raster
def readraster(rasterfile):   
    f = open(rasterfile)
    deltas = np.empty((703,803))  
    s = f.readline()
    for i in range(703):
        for j in range(803):
            deltas[i,j] = float(s)
            s = f.readline()
    f.close()
    return deltas
#
# convert datum from MSL to NAVD-88 by interpolating from raster    
def interpolate(xres, yres):
    xmin, xmax, ymin, ymax = -79.11, -75.10, 33.355, 36.865 
    ##coordinates defining rectangle of raster
    for i in range(len(var)):   
        if xmin <= lon[i] <= xmax and ymin <= lat[i] <= ymax:
            col1 = int((lon[i] - xmin)//xres)
            row1 = int((ymax - lat[i])//yres)
            col2 = col1 + 1
            row2 = row1 + 1
            x1 = xmin + col1*xres
            y1 = ymax - row1*yres
            x2 = x1 + xres
            y2 = y1 - yres
            if lat[i] == y2 and lon[i] == x1:
                avgdelta = deltas[row2,col1] 
            elif lat[i] == y2 and lon[i] == x2:
                avgdelta = deltas[row2,col2] 
            elif lat[i] == y1 and lon[i] == x1:
                avgdelta = deltas[row1,col1]                
            elif lat[i] == y1 and lon[i] == x2:
                avgdelta = deltas[row1,col2]
            else:
                dll = ((lon[i] - x1)**2 + (lat[i] - y2)**2)**0.5
                dlr = ((lon[i] - x2)**2 + (lat[i] - y2)**2)**0.5
                dul = ((lon[i] - x1)**2 + (lat[i] - y1)**2)**0.5
                dur = ((lon[i] - x2)**2 + (lat[i] - y1)**2)**0.5
                ##dll=distance to lower left corner of raster cell, etc.                
                if lat[i] == y1:
                    avgdelta = (deltas[row1,col1]/dul + deltas[row1,col2]/dur)/(1/dul + 1/dur)
                elif lat[i] == y2:
                    avgdelta = (deltas[row2,col1]/dll + deltas[row2,col2]/dlr)/(1/dll + 1/dlr)
                elif lon[i] == x1: 
                    avgdelta = (deltas[row1,col1]/dul + deltas[row2,col1]/dll)/(1/dul + 1/dll)
                elif lon[i] == x2:
                    avgdelta = (deltas[row1,col2]/dur + deltas[row2,col2]/dlr)/(1/dur + 1/dlr)
                else:                    
                    avgdelta = (deltas[row2,col1]/dll + deltas[row2,col2]/dlr + deltas[row1,col1]/dul + deltas[row1,col2]/dur)/(1/dll + 1/dlr + 1/dul + 1/dur)
        else:
            avgdelta = 0
        var[i] = var[i] - avgdelta
    return var
#
if datumconv == 'yes':
    time1 = time.time()    
    deltas = readraster(datumtextfile)
    var = interpolate(0.005,0.005)
    print 'Time to run datum conversion: ', time.time() - time1
#
# Extracting time step information from the output file 
time_var= nc['time']

#
# startdate specifies start time of ADCIRC computations 
startdate = time_var.units
#
# Converting the time step information into datetime objects 
dtime = netCDF4.num2date(time_var[:],startdate)

#
# Converting datetime objects to string format - YYMMDDHHMMSS 
a = []
for j in dtime:
    a.append(j.strftime("%Y%m%d%H%M%S"))
#
# FUNCTION TO CHECK IF VERTEX FALLS WITHIN A SPECIFIED LONG/LAT BOX##
def vertexcheck(LatN,LatS,LongE,LongW,vertex):
    if lon[vertex]<LongE and lon[vertex]>LongW and lat[vertex]>LatS and lat[vertex]<LatN:
        return True

## FUNCTION TO CREATE LOCAL MESH WITHIN SPECIFIED LONG/LAT BOX ##
def createSubmeshWithinSpecifiedLatLonBox(LatN,LatS,LongE,LongW,lonlatbuffer):
    nvertex= len(lon)
    nele = len(nv)
    locallat = []
    locallon = []
    varlocal = []
    localelem = []
    global2local = {}
    includeele1 = []
    includeele2 = []
    includevertex1 = []
    includevertex2 =[]
    includeele1 = [0]*nele
    includeele2 = [0]*nele
    includevertex1 = [0]*nvertex
    includevertex2 =[0]*nvertex
    LatN = LatN+lonlatbuffer
    LatS = LatS-lonlatbuffer
    LongE = LongE-lonlatbuffer
    LongW = LongW-lonlatbuffer
    for i in range(len(nv)):
        for j in nv[i]:
            if vertexcheck(LatN,LatS,LongE,LongW,j):
                includeele1[i] = 1
                includevertex1[nv[i][0]] = 1
                includevertex1[nv[i][1]] = 1
                includevertex1[nv[i][2]] = 1
                break
    for i in range(len(nv)):
        for j in nv[i]:
            if includevertex1[j] == 1:
                includeele2[i] = 1
                includevertex2[nv[i][0]] = 1
                includevertex2[nv[i][1]] = 1
                includevertex2[nv[i][2]] = 1
                break
    j = -1
    for i in range(len(lon)):
        if includevertex2[i] == 1:
            j = j+1
            global2local[i] = j
            locallat.append(lat[i])
            locallon.append(lon[i])
            varlocal.append(var[i])
                
    for i in range(len(nv)):
        if includeele2[i] == 1:
            localelem.append((global2local[nv[i][0]],global2local[nv[i][1]],global2local[nv[i][2]]))                   
    return locallat,locallon,localelem,varlocal
#
# READING IN A COLOR PALETTE FILE (.PAL) 
linenum = 0
palette = dict()
palette['value'] = []
palette['r'] = []
palette['g'] = []
palette['b'] = []
if viztype ==  'kmz':
    with open(palettename,'r') as file:      
        for line in file:
            linenum = linenum + 1
            if linenum >= 5:
                a = line.split(" ")
                palette['value'].append(float(a[0]))
                palette['r'].append(float(a[1]))
                palette['g'].append(float(a[2]))
                palette['b'].append(float(a[3]))
#
# Interpolating the color at each contour level from the palette 
def interpolateContourLevels(contourLevels,palette):
    contourPalette = dict()
    contourPalette['value'] = []
    contourPalette['r'] = []
    contourPalette['g'] = []
    contourPalette['b'] = []
    scale = []
    for i in range(len(contourLevels)):
        scale.append((contourLevels[i]-contourLevels[0])/(contourLevels[-1]-contourLevels[0]))
    for i in range(len(contourLevels)):
        #print scale[i]
        contourPalette['value'].append(scale[i])
        for j in range(len(palette['value'])):
            if scale[i] == palette['value'][j]:
                contourPalette['r'].append(palette['r'][j])
                contourPalette['g'].append(palette['g'][j])
                contourPalette['b'].append(palette['b'][j])
                break
            elif scale[i] > palette['value'][j] and scale[i] < palette['value'][j+1]:
                contourPalette['r'].append(np.around(palette['r'][j] + ((palette['r'][j+1]-palette['r'][j])/(palette['value'][j+1]-palette['value'][j]))*(scale[i]-palette['value'][j])))
                contourPalette['g'].append(np.around(palette['g'][j] + ((palette['g'][j+1]-palette['g'][j])/(palette['value'][j+1]
                                            -palette['value'][j]))*(scale[i]-palette['value'][j])))
                contourPalette['b'].append(np.around(palette['b'][j] + ((palette['b'][j+1]-palette['b'][j])/(palette['value'][j+1]
                                            -palette['value'][j]))*(scale[i]-palette['value'][j])))
                break
    RGB = list()
    for i in range(len(contourPalette['r'])):
        RGB.append((contourPalette['r'][i],contourPalette['g'][i],contourPalette['b'][i]))
    return RGB
#
# TO CONVERT FROM RGB COLOR CODE TO HEX COLOR CODE 
def rgb_to_hex(rgb):
    return '#%02x%02x%02x' % rgb
#
# CREATE COLOR BAR FOR THE CONTOUR PLOT 
def createColorBar(hexlist,levels,ticks):
    fig = pplot.figure(figsize = (1.5,8))
    ax = fig.add_axes([0.3,0.05,0.3,0.9])
    cmap = matplotlib.colors.ListedColormap(hexlist[:(len(hexlist)-1)],'contour')
    norm = matplotlib.colors.BoundaryNorm(levels, cmap.N)
    cb = matplotlib.colorbar.ColorbarBase(ax, cmap = cmap,norm=norm,boundaries=levels,extend='neither',ticks=ticks,spacing='proportional',orientation='vertical')
    cb.set_label(fileTypesUnitLabels[filetype])
    matplotlib.pyplot.savefig(fileTypesColorBarNames[filetype])

#
# CREATING SCREEN OVERLAYS (COLOR BAR AND LOGO) FOR THE KML FILE 
def createScreenOverlaysForKML(kml,logofile,logodims,logounits):
    screen1 = kml.newscreenoverlay(name='Colorbar')
    screen1.icon.href = fileTypesColorBarNames[filetype]
    screen1.overlayxy = simplekml.OverlayXY(x=0,y=0,xunits=simplekml.Units.fraction,
                                     yunits=simplekml.Units.fraction)
    screen1.screenxy = simplekml.ScreenXY(x=0,y=0.1,xunits=simplekml.Units.fraction,
                                     yunits=simplekml.Units.fraction)
    screen1.size.x = 0.1
    screen1.size.y = 0.72
    screen1.size.xunits = simplekml.Units.fraction
    screen1.size.yunits = simplekml.Units.fraction

    if logofile != 'null' and logofile != 'none': 
        screen2 = kml.newscreenoverlay(name='logo')
        screen2.icon.href = logofile
        screen2.overlayxy = simplekml.OverlayXY(x=0,y=1,xunits=simplekml.Units.fraction,
                                       yunits=simplekml.Units.fraction)
        screen2.screenxy = simplekml.ScreenXY(x=0,y=1,xunits=simplekml.Units.fraction,
                                        yunits=simplekml.Units.fraction)
        if logounits == 'fraction':     
            screen2.size.xunits = simplekml.Units.fraction
            screen2.size.yunits = simplekml.Units.fraction
        if logounits == 'pixel':
            screen2.size.xunits = simplekml.Units.pixel
            screen2.size.yunits = simplekml.Units.pixel
        if logodims == 'null':
            screen2.size.x = 0.85
            screen2.size.y = 0.08
        else:
            a = logodims.split(" ")
            screen2.size.x = float(a[0])
            screen2.size.y = float(a[1])
#
# GENERAL REQUIREMENTS FOR SHAPE FILES AND KML FILES 
if viztype ==  'kmz':
    ## CREATING A SIMPLEKML OBJECT ##
    kml = simplekml.Kml()
    box = simplekml.Box(north = 46.86, south = 4.32, east = -57.523, west = -90)
    lod = simplekml.Lod(minlodpixels=128, maxlodpixels=-1, minfadeextent=0, maxfadeextent=0)
    reg = simplekml.Region(box,lod)
    c = interpolateContourLevels(levels,palette)
    hexColorsList = []
    for i in c:
        hexColorsList.append(rgb_to_hex(i))
    createColorBar(hexColorsList,levels,ticks)
    createScreenOverlaysForKML(kml,logofile,logodims,logounits)
else:
    ## DEFINING SPATIAL REFERENCE ##
    crs = {'no_defs': True, 'ellps': 'WGS84', 'datum': 'WGS84', 'proj': 'longlat'}
    ## DEFINING OGR DRIVER ##
    driver = 'ESRI Shapefile'
#
# Triangulating the entire domain ##
tri = matplotlib.tri.Triangulation(lon,lat,triangles=nv)
#
# geoms is an ordered dictionary which stores the details of geometry 
#of the polygons which describe the individual contour levels #
geoms = collections.OrderedDict()

def classifyPolygons(polys):
    outer = []
    inner = []
    for p in polys:
        if signedArea(p) >= 0:
            outer.append(p)
        else:
            inner.append(p)
    return outer,inner

def pointsInsidePoly(points, polygon):
    p = matplotlib.path.Path(polygon)
    return p.contains_points(points)

def reverseGeometry(p):
  return np.flipud(p)

## To calculate the signed area of an irregular polyon ##
def signedArea(ring):
    """Return the signed area enclosed by a ring in linear time using the 
    algorithm at: https://web.archive.org/web/20080209143651/http://cgafaq.info:80/wiki/Polygon_Area.
    """
    v2 = np.roll(ring, -1, axis=0)
    return np.cross(ring, v2).sum() / 2.0
     
## CREATING CONTOUR AND EXTRACTING LINESTRINGS/POLYGONS FOR EVERY TIMESTEP ##
for i in range(len(time_var)):
    print "Time step " + str(i)
    if viztype ==  'kmz':
        fol = kml.newfolder(name = fileTypesKMLFolderNames[filetype])

        fol.region = reg
        store = {}
        for k in range(len(levels)):
            multipol = fol.newmultigeometry(name= 'Level' + str(k))
            store[k] = multipol
    if filetype == 'fort.74.nc':
        wind[i] = []
        for j in range(len(windx[i])):
            wind[i].append(math.sqrt(windx[i][j]**2+windy[i][j]**2))
    if viztype ==  'kmz' and subplots == 'yes' and polytype == 'polygon':
        ## Creating polygon KML files by combining contour plots of subdomains specified in bins ##
        for v in bins:
            ## Printing the long/lat box ##       
            #print 'bin is ' + str(v)
            localy = []
            localx =[]
            localelements = []
            ## Extracting local mesh for each long/latbox ## 
            localy,localx,localelements,localvar = createSubmeshWithinSpecifiedLatLonBox(v[0],v[1],v[2],v[3],lonlatbuffer)
            if localelements ==[]:
                #print localelements
                continue
            ## Triangulating for each local mesh ##
            tri = matplotlib.tri.Triangulation(localx,localy,triangles=localelements)
            ## Plotting filled contour for each local mesh ##
            print 'INFO: Plotting filled contour for each local mesh.'
            #print 'localvar is ' + str(localvar)
            contour = pplot.tricontourf(tri, localvar,levels=levels,shading='faceted')          
            m = 0           
            for colli,coll in enumerate(contour.collections):
                vmin,vmax = contour.levels[colli:colli+2]
                #print 'Level %d' %m
                #print vmin,vmax
                ## Extracting the path objects corresponding to each contour level ##         
                for p in coll.get_paths():
                    p.simplify_threshold = 0.0
                    ## converting each path object to a set of polygons  - polys ##
                    ## Deleting any polygons having less than 3 vertices ## 
                    polys = [g for g in p.to_polygons() if g.shape[0] >=3] 
                    if len(polys)>0:
                        ## polys is converted to a list of tuples (polys1). Each tuple representing a polygon ##
                        polys1 = []
                        for g in polys:
                             polys1.append(list(map(tuple,g)))   
                        inner = []
                        outer = []
                        ## Finding out which of the polygons within polys1 are outer and inner polygons (or holes)
                        ## by calculating their signed area. If area is positive it is an outer polygon and if it is negative
                        ## it is a inner polygon
                        for i in range(len(polys1)):
                            polys1[i] = LinearRing(polys[i])
                            s = float(1.0)
                            if signedArea(polys1[i])/s >=0.0:
                                outer.append(list(polys1[i].coords))
                            else:
                                inner.append(list(polys1[i].coords))
                        ## Need to identify which are the inner polygons (from inner) which fall inside each of the outer polygons  (from outer).
                        ## topo = {<index of Outer Polygon 1 in outer>:<index of polygon in inner which falls in this outer polygon>, <index of Outer Polygon 2 in outer>: <.......>, ...}         
                        topo = {}
                        for i in range(len(outer)):
                            topo[i] = []
                            for j in range(len(inner)):
                                inside = 0
                                for k in inner[j]:
                                    if Polygon(outer[i]).contains(Point(k)):
                                        inside = 1
                                        break
                                    else:
                                        inside = 0
                                        break
                                if inside == 1:
                                    topo[i].append(j)
                            pol = store[m].newpolygon(name= 'Level' + str(m))
                            pol.outerboundaryis = outer[i]
                            if topo[i]!= []:
                                holes = []
                                for k in topo[i]:
                                    holes.append(inner[k])
                                pol.innerboundaryis = holes[:]
                            pol.gxaltitudemode = simplekml.GxAltitudeMode.clampToSeaFloor
                ## Setting the kml multigeometry object properties ##
                store[m].visibility = 1
                store[m].style.polystyle.fill = 1
                #print m
                store[m].style.polystyle.color = simplekml.Color.hex(hexColorsList[m][1:])
                store[m].style.polystyle.outline = 0
                m = m+1               
    else:
        ## Writing shapefiles/KML files  for entire domain ##
        ## Writing KML files for entire domain in one step may not allow polygons to be rendered correctly ##
        if polytype == 'polyline':
            ## To create POLYLINE files ##
            if filetype in timeVaryingFiles:
                var = nc[fileTypesNetCDFVarNames[filetype]][i][:]
                # convert units if necessary
                if units == 'english':
                   var = np.multiply(float(fileTypesEnglishUnitsConversions[filetype]),var)
            contour = pplot.tricontour(tri, var,levels=levels)
            geoms[time_var[i]] = []
            print time_var[i]
            m = 0
            for colli,coll in enumerate(contour.collections):
                val = contour.levels[colli]
                print 'Level %d' %m
                print val
                for pp in coll.get_paths():
                    if len(pp.vertices) > 1:
                        ## Extracting the vertices of each of the paths corrsponding to each contour level ##
                        ## and wrapping them up in a shapely Linestring object ##
                        geoms[time_var[i]].append((LineString(pp.vertices),val))
                m = m + 1
        elif polytype == 'polygon':
            ## To create POLYGON files ##
                    if filetype in timeVaryingFiles:
                        var = nc[fileTypesNetCDFVarNames[filetype]][i][:]
                    # construct mask
                    if filetype in requireFill and type(var) == "MaskedArray" and var.mask.any():
                        point_mask_indices = np.where(var.mask)
                        tri_mask = np.any(np.in1d(nv,point_mask_indices).reshape(-1,3),axis=1)
                        tri.set_mask(tri_mask)                
                    # convert units if necessary
                    if filetype in timeVaryingFiles:
                        if units == 'english':
                            var = np.multiply(float(fileTypesEnglishUnitsConversions[filetype]),var)
                        # limit range to the contour range
                        np.place(var,var > max(levels),max(levels)-0.01)   
                        np.place(var,(-100 < var) & (var < min(levels)),min(levels)) 
                    contour = pplot.tricontourf(tri, var,levels=levels)
                    geoms[time_var[i]] = []
                    m = 0
                    l = len(contour.collections)
                    for colli,coll in enumerate(contour.collections):
                        vmin,vmax = contour.levels[colli:colli+2]
                        if viztype ==  'kmz':
                           multipol = fol.newmultigeometry(name= 'Level' + str(m))                         
                        for p in coll.get_paths():
                            p.simplify_threshold = 0.0
                            # 
                            # Removing polygons which have less than three 
                            # coordinates to describe its boundary
                            polys = [g for g in p.to_polygons() if g.shape[0] >=3] 
                            #
                            # Extracting the vertices of each of the paths 
                            # corrsponding to each contour level and wrapping 
                            # them up in a shapely Polygon object 
                            #
                            # The geometry information with the contour 
                            # levels is stored in geoms
                            outer,inner = classifyPolygons(polys)
                            if len(inner)>0:
                                inner_points = [pts[0] for pts in inner]
                            overall_inout = np.zeros((len(inner),),dtype = np.bool)
                            
                            for out in outer:
                                if len(inner) > 0:
                                    inout = pointsInsidePoly(inner_points,out)
                                    overall_inout = np.logical_or(overall_inout, inout)
                                    out_inner = [g for f, g in enumerate(inner) if inout[f]]
                                    poly = Polygon(out, out_inner)
                                else:
                                    poly = Polygon(out)

                                # clean-up polygons (remove intersections)
                                if not poly.is_valid:
                                    poly = poly.buffer(0.0)
                                if poly.is_empty:
                                    continue
                                geoms[time_var[i]].append((poly,vmin,vmax))
                            # collect all interiors which do not belong to any of the exteriors
                            outer_interiors = [interior for s,interior in enumerate(inner) if not overall_inout[s]]
                            for k in outer_interiors:
                                poly = Polygon(reverseGeometry(k))
                                # clean-up polygons (remove intersections)
                                if not poly.is_valid:
                                    poly = poly.buffer(0.0)
                                if poly.is_empty:
                                    continue
                                geoms[time_var[i]].append((poly,vmin,vmax))
                            #print len(geoms[time_var[i]])
                            if viztype ==  'kmz':
                                ## Creating kml files for the whole domain - this is not recommended for very fne meshes ##
                                ## due to the restrictions on the maximum number of vertices (31000) for a kml polygon object. ##
                                ## This restriction does not allow the polygons to be plotted correctly ##
                                # Can't use i again for counting variable. it is already the counting variable for time_var loop.
                                for ii in range(len(polys)):
                                    polys[ii] = np.vstack([polys[ii],polys[ii][0]])
                                    pol = multipol.newpolygon(name = 'Polygon'+str(ii))
                                    pol.outerboundaryis = polys[ii]
                                multipol.visibility = 1
                                multipol.style.polystyle.fill = 1
                                multipol.style.polystyle.color = simplekml.Color.hex(hexColorsList[m][1:])
                                multipol.style.polystyle.outline = 0
                                s = "       min = " + str(vmin) + " ft, max = " +str(vmax) +" ft"
                                pol.style.balloonstyle.text = s
                                pol.style.balloonstyle.bgcolor = simplekml.Color.brown
                                pol.style.balloonstyle.textcolor = simplekml.Color.black
                                pol.description = s
                        m = m+1                    
#
# KEEPING TRACK OF RUNNING TIME ##
if  viztype ==  'shapefile':
    print 'Finished contouring, extracting information and creating polygons after %d seconds'% (time.time()-time0)
else:
    print 'Finished contouring, extracting information and creating multigeometry polygons after %d seconds'% (time.time()-time0)
#
fileTypesShapeVarNames = { 'bathytopo' : 'waterdepth', 'maxele.63.nc' : 'maxelev', 'maxwvel.63.nc' : 'maxwvel', 'swan_HS_max.63.nc' : 'maxwvht', 'fort.63.nc' : 'elevat', 'fort.74.nc' : 'wvel', 'swan_HS.63.nc' : 'wavht' }
#
fileTypesShapeVarPrefixes = { 'bathytopo' : 'd', 'maxele.63.nc' : 'ele', 'maxwvel.63.nc' : 'wvel', 'swan_HS_max.63.nc' : 'wvht', 'fort.63.nc' : 'ele', 'fort.74.nc' : 'wvel', 'swan_HS.63.nc' : 'wavht', 'swan_TPS.63.nc' : 'wvpd', 'swan_TPS_max.63.nc' : 'peakpd', 'swan_TMM10.63.nc' : 'meanpd' }
#
shapeVarSuffixes = [ 'min', 'max', 'avg' ]
#
#
# DEFINING SCHEMA & WRITING SHAPE FILE
if viztype ==  'shapefile':
    # use default shapefile names if nothing else was specified
    if outputfile == 'null':
        outputname = fileTypesDefaultOutputShapeFileNames[filetype]
    else:
        outputname = outputfile
    if polytype == 'polyline':
        if filetype in timeVaryingFiles:           
            schema = { 'geometry': 'LineString', 'properties': { fileTypesShapeVarNames[filetype] : 'float','timestep':'str'}}
            with fiona.open(outputname, 'w',driver,schema,crs) as c:
                for geom in geoms:
                    k = list(time_var).index(geom)
                    print k
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': { fileTypesShapeVarNames[filetype] : g[1],'timestep':a[k]}})
        else:
            schema = { 'geometry': 'LineString', 'properties': { fileTypesShapeVarNames[filetype] : 'float'}}
            with fiona.open(outputname, 'w',driver,schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': { fileTypesShapeVarNames[filetype] : g[1]}})

    elif polytype == 'polygon':
        # create the list of variable names for min max and average
        shapeVars = [ fileTypesShapeVarPrefixes[filetype] + x for x in shapeVarSuffixes ]  
        if filetype in timeVaryingFiles:           
            schema = { 'geometry': 'Polygon', 'properties': { shapeVars[0] : 'float', shapeVars[1]: 'float', shapeVars[2] : 'float','timestep':'str' ,'t' : 'float'} }
            with fiona.open(outputname, 'w', driver, schema,crs) as c:
                for geom in geoms:
                    k = list(time_var).index(geom)
                    print k
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': { shapeVars[0]: g[1], shapeVars[1] : g[2], shapeVars[2] : (g[1]+g[2])/2.0,'timestep':a[k],'t':time_var[k]}})
        else:
            schema = { 'geometry': 'Polygon', 'properties': { shapeVars[0]: 'float', shapeVars[1]: 'float', shapeVars[2]:'float'}}
            with fiona.open(outputname, 'w',driver,schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {shapeVars[0]: g[1], shapeVars[1]: g[2], shapeVars[2]: (g[1]+g[2])/2.0}})
                        
elif viztype ==  'kmz':
    # Specifying the kml file name and saving file 
    # use default kml name if nothing else was specified
    if outputfile == 'null':
        kmlFileName = fileTypesKMLFolderNames[filetype] +'.kml'       
    else:
        kmlFileName = outputfile + '.kml'
    kml.save(kmlFileName)

    
## KEEPING TRACK OF RUNNING TIME ##
if viztype ==  'shapefile':
    print 'Finished generating shapefile after  %d seconds'% (time.time()-time0)
else:
    print 'Finished generating KML file after %d seconds'% (time.time()-time0)

## R.GROW OPTION ##
if grow == 'yes':
    os.system("unzip -q GRASS_LOCATION.zip")
    os.system("unzip -q GRASS_LOCATION_wgs84.zip")

    #Modification for automated ncsu-viz system:
    #Imports rgrow_pll.inp file to read information necessary for use with ncsu-viz-mailer.sh

    inputList=[]
    with open("rgrow_pll.inp","r") as r:
        for line in r:
            inputList=line.split()
    outFileFolder="{0}_{1}_{2}_{3}_{4}_enhanced".format(inputList[1],inputList[2],inputList[3],inputList[4],inputList[5])

    #Working with GRASS without starting it explicitly; using existing location.
    #This works assuming a linux operating system. If using a different operating system, see the website below.
    #More information: https://grasswiki.osgeo.org/wiki/Working_with_GRASS_without_starting_it_explicitly

    grass7bin = 'grass72'
    gisdb = os.path.expanduser("./")
    startcmd = [grass7bin, '--config', 'path']
    p = subprocess.Popen(startcmd, shell=False, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    if p.returncode !=0:
	print >>sys.stderr, 'ERROR: Cannot find GRASS GIS 7 start script (%s)' % startcmd
	sys.exit(-1)
    gisbase = out.strip('\n\r')
    os.environ['GISBASE'] = gisbase
    os.environ['PATH'] += os.pathsep + os.path.join(gisbase, 'extrabin')
    home = os.path.expanduser('./')
    os.environ['PATH'] += os.pathsep + os.path.join(home, '.grass7', 'addons', 'scripts')
    gpydir = os.path.join(gisbase, "etc", "python")
    sys.path.append(gpydir)
    os.environ['GISDBASE'] = gisdb

    #GRASS is now ready to be imported

    import grass.script as grass
    import grass.script.setup as gsetup
    from grass.exceptions import CalledModuleError

    location = 'GRASS_LOCATION_wgs84'
    mapset = 'PERMANENT'
    gsetup.init(gisbase, gisdb, location, mapset)#Setting the GRASS working environment to GRASS_LOCATION_wgs84 (WGS84;lat/long)

    print 'Finished setting up GRASS environment after {0:.2f} seconds'.format(time.time()-time0)

    #import_kalpana.py begins:#######################################################

    #Importing the shapefile generated by Kalpana
    grass.run_command('v.in.ogr',
	input='./kalpana_out/kalpana_out.shp',
	min_area=10,
	snap=0.000001,
	overwrite=True,
	quiet=True)

    #kalpana_vtorast.py begins:######################################################
    location2 = 'GRASS_LOCATION'

    gsetup.init(gisbase, gisdb, location2, mapset)#Setting the GRASS working environment to GRASS_LOCATION (stateplane;ft or m)

    #Takes shapefile from WGS84 location and projects it into GRASS_LOCATION.
    grass.run_command('v.proj',
	location='GRASS_LOCATION_wgs84',
	mapset='PERMANENT',
	input='kalpana_out@PERMANENT',
	output='kalpana_vproj',
	overwrite=True,
	quiet=True)

    #Read EW and NS resolution
    reg=grass.read_command('g.region',flags='g').split()
    ewresReg=float(reg[7].split("=")[1])
    nsresReg=float(reg[6].split("=")[1])

    #Setting computational region based on DEM.
    grass.run_command('g.region',
	raster='dem@PERMANENT',
        nsres=nsresReg,
        ewres=ewresReg,
	overwrite=True,
	quiet=True)
    #Converts shapefile to raster using eleavg (average elevation per Kalpana bin).
    #Other options include elemax and elemin.
    grass.run_command('v.to.rast',
	input='kalpana_vproj',
	type='area',
	output='kalpana_rast',
	use='attr',
	attribute_column='eleavg',
	overwrite=True,
	quiet=True)

    #Creates mask based on extents of DEM raster to limit area of raster operation.
    grass.run_command('r.mask',
	raster='dem@PERMANENT',
	quiet=True,
	overwrite=True)

    print 'Finished converting water levels to raster format after {0:.2f} minutes'.format((time.time()-time0)/60)

    #Intermediate r.grow.modified steps pasted below:####################################

    # what to do in case of user break: <--Not sure if this is still needed since the locations are being deleted at the end
    def cleanup():
	gsetup.init(gisbase, gisdb, location2, mapset)
	for map in [temp_dist, temp_val]:
	    if map:
		grass.run_command('g.remove', flags='fb', quiet=True,
				  type='rast', name=map)

    #atexit.register(cleanup)

    global temp_dist, temp_val

    input = 'kalpana_rast@PERMANENT'
    output = 'WaterLevels_final_binned'
    base = 'dem@PERMANENT'
#    radius = options.growradius
#    if radius!="none":
#        radius=float(radius)
#    metric = 'euclidean'
    old = ''
    new = ''
    mapunits = ''

    tmp = str(os.getpid())

    temp_dist = "r.grow.tmp.%s.dist" % tmp

    shrink = False
#    if radius < 0.0 and radius != "none":
#	shrink = True
#    if radius != "none":
#        radius = -radius

    if new == '' and shrink == False:
	temp_val = "r.grow.tmp.%s.val" % tmp
	new = temp_val
    else:
	temp_val = None

    if old == '':
	old = input

#    if not mapunits:
#	kv = grass.region()
#	scale = math.sqrt(float(kv['nsres']) * float(kv['ewres']))
#	radius *= scale

#    if metric == 'euclidean':
#	metric = 'squared'
#	radius = radius * radius

    # check if input file exists
    if not grass.find_file(input)['file']:
	grass.fatal(_("Raster map <%s> not found") % input)

    if not grass.find_file(base)['file']:
	grass.fatal(_("Basemap <%s> not found") % base)

    if shrink == False:
	try:
            grass.run_command('r.grow.distance',input=input,value=temp_val)
#	    grass.run_command('r.grow.distance', input=input, metric=metric,
#			      distance=temp_dist, value=temp_val)
	except CalledModuleError:
	    grass.fatal(_("Growing failed. Removing temporary maps."))

	grass.mapcalc(
            "$output = if(!isnull($input),$old,if($base < $new,$new,null()))",
            output=output, input=input, base=base, old=old, new=new)
#	    "$output = if(!isnull($input),$old,if($dist < $radius && $base < $new,$new,null()))",
#	    output=output, input=input, radius=radius, base=base,
#	    old=old, new=new, dist=temp_dist)
    #BLOCK NOT NEEDED in this version
    else:
	# shrink
	try:
	    grass.run_command('r.grow.distance', input=input, metric=metric,
			      distance=temp_dist, value=temp_val, flags='n')
	except CalledModuleError:
	    grass.fatal(_("Shrinking failed. Removing temporary maps."))

	grass.mapcalc(
	    "$output = if($dist < $radius,null(),$old)",
	    output=output, radius=radius, old=old, dist=temp_dist)

    # grass.run_command('r.colors', map=output, raster=input)

    # write cmd history:
    #grass.raster_history(output)

    print 'Finished setting up data for r.grow process after {0:.2f} minutes'.format((time.time()-time0)/60)

    #Intermediate grow_process.py steps pasted below: ######################################

    # Gives all non-null cells in grown raster a single uniform value (-1).
    grass.mapcalc("$output = if(!isnull($input),-1,null())",
		  output='tempmap',
		  input='WaterLevels_final_binned',
		  quiet=True,
		  overwrite=True)

    # Groups the uniform raster by giving each connected group of cells a unique ID.
    # The goal is to remove isolated clumps not connected to original raster.
    grass.run_command('r.clump',
		      input='tempmap',
		      output='clumpmap',
		      quiet=True,
		      overwrite=True)

    # Identifies original clumps found in the ADCIRC raster.
    grass.mapcalc("$output = if(!isnull($A) && !isnull($B),$B,null())",
		  output='tempmap',
		  A='kalpana_rast',
		  B='clumpmap',
		  quiet=True,
		  overwrite=True)

    # Sorts clump areas largest to smallest, removes largest clump (null clump).
    # List format: [clump1, #cells1, clump2, #cells2, ...].
    areas=grass.read_command('r.stats',
			     input='tempmap',
			     sort='desc',
			     flags='c',
			     quiet=True).split()
    if areas[0] == '*':
	areas = areas[2:]
    else:
	areas.remove(areas[2])
	areas.remove(areas[2])

    # Loops over #cells in each clump, starting with largest. Stops when #cells < 50
    # and deletes all remaining smaller clumps. This step is necessary only because
    # the following step cannot handle such a list of areas. Vast majority of areas
    # that will be kept are connected to the large, original ADCIRC raster.
    cutoff = len(areas)
    for i in range(1,len(areas),2):
	if int(areas[i]) < 50:
	    cutoff = i - 1
	    break
    areas = areas[:cutoff]

    # Assigns uniform value (-1) to all areas that include and are connected to
    # original ADCIRC raster (r.reclass requires a list of rules as str)
    reclasslist = ''
    for i in range(0,len(areas),2):
	reclasslist += areas[i] + ' '
    reclasslist += '= -1'
    grass.write_command('r.reclass',
			input='clumpmap',
			output='tempmap',
			rules='-',
			stdin=reclasslist,
			quiet=True,
			overwrite=True)

    # Passes back grown ADCIRC cells if they coincide with the assigned value (-1).
    grass.mapcalc("$output = if($A == -1,$B,null())",

		  output='storm_final',
		  A='tempmap',
		  B='WaterLevels_final_binned',
		  quiet=True,
		  overwrite=True)

    # Keyword 'with' means ADCIRC cells with values lower than the land surface
    # elevation will be removed
    if growmethod == 'with':
	grass.mapcalc("$output = if($adcirc < $dem, null(), $adcirc)",
		      output='storm_final_less_errors',
		      adcirc='storm_final',
		      dem='dem',
		      quiet=True,
		      overwrite=True)
	grass.run_command('g.rename',
		      raster=('storm_final_less_errors','storm_final'),
		      overwrite=True,
		      quiet=True)

    if flooddepth != "yes":
        # Rounds each new ADCIRC cell value to the nearest 0.5
        grass.mapcalc("$output = if($adcirc - int($adcirc) > 0.5,int($adcirc) + 1.0,int($adcirc) + 0.5)",
	    	      output='storm_final_binned',
		      adcirc='storm_final',
		      quiet=True,
		      overwrite=True)

    print 'Finished executing r.grow after {0:.2f} minutes'.format((time.time()-time0)/60)

    #Flood depth visualization option
    if flooddepth == "yes":
	#Take the difference between flood elevation and land elevation where flooding occurs
	grass.mapcalc("flood_depth=if(dem>0,storm_final-dem,null())",
	    overwrite=True)
	grass.run_command('r.out.gdal',
	    input='flood_depth',
	    flags='m',
	    format='GTiff',
	    nodata=-9999,
	    output=grownoutput+'.tif',
	    overwrite=True)
	print 'Finished creating {0}.tif after {1:.2f} minutes'.format(grownoutput,float((time.time()-time0)/60))

    else:
        #Converts binned raster to polygons
        grass.run_command('r.to.vect',
	  	          input='storm_final_binned',
		          output=outFileFolder,
		          type='area',
		          flags='s',
		          quiet=True,
		          overwrite=True)

        #Export to a useful format specified in grownfiletype; default is ESRI shapefile
        grass.run_command('v.out.ogr',
		          input=outFileFolder,
		          output=outFileFolder,
		          type='area',
		          format=grownfiletype,
		          flags='se',
		          quiet=True,
		          overwrite=True)

        print 'Finished creating {0} after {1:.2f} minutes'.format(outFileFolder,float((time.time()-time0)/60))

### The following is not necessary for mailer process:
###	#Zip output and remove extranous folders ###Pay attention to output type; it's possible not all grownfiletypes can be zipped.
    #os.system("zip -r {0} {1}".format(outFileZip,outFileFolder)
    #os.system("rm -fr GRASS_LOCATION_wgs84 GRASS_LOCATION kalpana_out {0}".format(outFileFolder)
    #if grownfiletype != 'ESRI_Shapefile':
        #print 'USER WARNING: User may be required to specify file extension for {0}'.format(outFileFolder)

