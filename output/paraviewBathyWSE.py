#!/usr/bin/python
#----------------------------------------------------------------------
# paraviewBathyWSE.py : Visualize bathy and wse simultaneously in 
# Paraview.
#----------------------------------------------------------------------
# Copyright(C) 2016 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------------
from optparse import OptionParser
try: paraview.simple
except: from paraview.simple import *
paraview.simple._DisableFirstRenderCameraReset()

#     C O M M A N D   L I N E   O P T I O N S
parser = OptionParser()
#parser.add_option("-i", "--interact", dest="interact", default=False,
#                  action="store_true", help="to enable interaction with data")
#parser.add_option("-o", "--outline", dest="outline", default=False,
#                  action="store_true", help="to display mesh outline")
parser.add_option("-f", "--frame", dest="frame", default=1,
                  help="frame to render")
parser.add_option("-m", "--magnification", dest="magnification", default=1,
                  help="magnification of output image (integer)")                  
#parser.add_option("-a", "--annotation", dest="annotation", default="null",
#                  help="text to place in frame")
(options, args) = parser.parse_args()

#             R E A D   D A T A  
fort_63_nc_xmf = XDMFReader( FileName='/home/jason/projects/UNC-ASGS/2016/TableTop/08/nhcConsensus/fort.63.nc.xmf' )
fort_63_nc_xmf.PointArrays = ['sea_surface_height_above_geoid', 'BathymetricDepth']

#      W A T E R   S U R F A C E   E L E V A T I O N

# set coloring for water surface elevation to ERDC rainbow (dark)
wseColorBar_PVLookupTable = GetLookupTableForArray( "sea_surface_height_above_geoid", 1, RGBPoints=[0.0, 0.0, 0.0, 0.423499, 0.6688949999999999, 0.0, 0.119341, 0.529244, 1.3377949999999998, 0.0, 0.238697, 0.634974, 2.00669, 0.0, 0.346853, 0.687877, 2.675585, 0.0, 0.450217, 0.718135, 3.34448, 0.0, 0.553552, 0.664836, 4.01338, 0.0, 0.651087, 0.51931, 4.682274, 0.115846, 0.724788, 0.35285, 5.3511705, 0.326772, 0.781201, 0.140185, 6.020065, 0.522759, 0.79852, 0.0284581, 6.688965, 0.703166, 0.788678, 0.00885023, 7.35786, 0.845121, 0.751141, 0.0, 8.026755, 0.955734, 0.690822, 0.0, 8.69565, 0.995407, 0.56791, 0.0618448, 9.36455, 0.987716, 0.403403, 0.164858, 10.0, 0.980407, 0.247105, 0.262699], VectorMode='Magnitude', NanColor=[0.498039, 0.0, 0.0], ColorSpace = 'Lab', ScalarRangeInitialized=1.0 )  
wseColorBar_PiecewiseFunction = CreatePiecewiseFunction( Points=[0.0, 0.0, 0.5, 0.0, 10.0, 1.0, 0.5, 0.0] )
wseColorBar_PVLookupTable.ScalarOpacityFunction = wseColorBar_PiecewiseFunction
wseColorBar_PVLookupTable.LockScalarRange = 1

# use threshold filter to elimitate the -99999 values from the water 
# surface elevation data
SetActiveSource(fort_63_nc_xmf) # start building the pipeline from the reader
Threshold1 = Threshold()
Threshold1.ThresholdRange = [-99998.0, 100.0]
Threshold1.Scalars = ['POINTS', 'sea_surface_height_above_geoid']

WarpByScalar1 = WarpByScalar()
WarpByScalar1.Scalars = ['POINTS', 'sea_surface_height_above_geoid']
WarpByScalar1.ScaleFactor = 0.0002

DataRepresentation1 = Show()
DataRepresentation1.ColorArrayName = ('POINT_DATA', 'sea_surface_height_above_geoid')
DataRepresentation1.ScalarOpacityFunction = wseColorBar_PiecewiseFunction
DataRepresentation1.LookupTable = wseColorBar_PVLookupTable

#    B A T H Y M E T R Y  /  T O P O G R A P H Y 

# need to remove dry areas that are below msl from the visualization
# otherwise they will show up blue in the visualization and look like 
# they are underwater
SetActiveSource(fort_63_nc_xmf) # start building the pipeline from the reader
Threshold2 = Threshold()
Threshold2.Scalars = ['POINTS', 'BathymetricDepth']
Threshold2.ThresholdRange = [-100.0, 0.0]

# use Casey's bathy/topo color bar 
bathyColorBar_PVLookupTable = GetLookupTableForArray( "BathymetricDepth", 1, RGBPoints=[-20.0, 0.0, 0.250004, 0.0, -10.0, 0.0, 0.500008, 0.0, -5.0, 0.0, 0.629999, 0.0, -2.0, 0.0, 0.764996, 0.0, -1.0, 0.0, 0.8, 0.0500038, -0.5, 0.0, 0.850004, 0.100008, -0.2, 0.0, 0.900008, 0.149996, -0.1, 0.0, 0.949996, 0.2, 0.0, 0.0, 1.0, 1.0, 0.0001, 1.0, 1.0, 1.0, 0.1, 1.0, 1.0, 1.0, 0.2, 0.0, 1.0, 1.0, 0.5, 0.0, 0.500008, 1.0, 1.0, 0.0, 0.4, 1.0, 2.0, 0.0, 0.299992, 1.0, 5.0, 0.0, 0.2, 1.0, 10.0, 0.0, 0.100008, 1.0, 20.0, 0.0, 0.00999466, 1.0, 50.0, 0.0, 0.0, 1.0, 100.0, 0.0, 0.0, 0.510002], VectorMode='Magnitude', NanColor=[0.498039, 0.0, 0.0], ColorSpace='RGB', ScalarRangeInitialized=1.0 )
bathyColorBar_PiecewiseFunction = CreatePiecewiseFunction( Points=[-66.632401, 0.0, 0.5, 0.0, 0.0, 1.0, 0.5, 0.0] )
bathyColorBar_PVLookupTable.ScalarOpacityFunction = bathyColorBar_PiecewiseFunction

WarpByScalar2 = WarpByScalar()
WarpByScalar2.Scalars = ['POINTS', 'BathymetricDepth']
WarpByScalar2.ScaleFactor = -0.0002

DataRepresentation5 = Show()
DataRepresentation5.EdgeColor = [0.0, 0.0, 0.5000076295109483]
DataRepresentation5.SelectionPointFieldDataArrayName = 'BathymetricDepth'
DataRepresentation5.ScalarOpacityFunction = bathyColorBar_PiecewiseFunction
DataRepresentation5.ColorArrayName = ('POINT_DATA', 'BathymetricDepth')
DataRepresentation5.ScalarOpacityUnitDistance = 0.11216901957450816
DataRepresentation5.LookupTable = bathyColorBar_PVLookupTable
DataRepresentation5.ScaleFactor = 1.2353294372558594

# T E X T   A N N O T A T I O N
#Text1 = Text()
#Text1.Text = 'Hurricane Zack Exercise\nNHC Official Forecast Track\nAdvisory 8'
#DataRepresentation6 = Show()

RenderView1 = GetRenderView()
RenderView1.CameraClippingRange = [102.0, 105.0]
RenderView1.CameraFocalPoint = [-90.5, 29.5, 0.0]
RenderView1.CenterOfRotation = [-90.5, 29.5, 0.0]
RenderView1.CameraParallelScale = 1.6
RenderView1.InteractionMode = '2D'
RenderView1.CameraPosition = [-90.5, 29.5, 103.0]
RenderView1.CenterAxesVisibility = 0 # turn off axes that show center of rotation

#save screenshot
view = GetActiveView()
view.Background = [0.35,0.36,0.45]  # dark gray
tsteps = fort_63_nc_xmf.TimestepValues
annTime = AnnotateTimeFilter(fort_63_nc_xmf)
# Show the filter
Show(annTime)
view.ViewTime = tsteps[int(options.frame)]
Render()
frame_file = 'test_%03d.png' % int(options.frame)
WriteImage(frame_file,Magnification=int(options.magnification))

#view.ViewTime = tsteps[100]
#Render()
#WriteImage("newtest3.png",Magnification=4)

# Save the animation to an avi file
#AnimateReader(fort_63_nc_xmf, filename="movie.avi")

