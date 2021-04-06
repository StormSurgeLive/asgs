#!/home/jason/ParaView-5.8.1-MPI-Linux-Python2.7-64bit/bin/pvbatch
from paraview.simple import *
from optparse import OptionParser
#
# parse command line options
parser = OptionParser()
parser.add_option("-i", "--interact", dest="interact", default=False,
                  action="store_true", help="to enable interaction with data")
parser.add_option("-o", "--outline", dest="outline", default=False,
                  action="store_true", help="to display mesh outline")
parser.add_option("-f", "--frame", dest="frame", default=1,
                  help="frame to render")
parser.add_option("-a", "--annotation", dest="annotation", default="null",
                  help="text to place in frame")

# trace generated using paraview version 5.8.1
#
# To ensure correct image size when batch processing, please search
# for and uncomment the line `# renderView*.ViewSize = [*,*]`

#### import the simple module from the paraview
from paraview.simple import *
#### disable automatic camera reset on 'Show'
paraview.simple._DisableFirstRenderCameraReset()

# create a new 'Legacy VTK Reader'
spatial_data_001d = LegacyVTKReader(FileNames=['/home/jason/projects/NCFS/WindTest/Irene/hindcast/temp/spatial_data_001.d'])

# get active view
renderView1_1 = GetActiveViewOrCreate('RenderView')
# uncomment following to set a specific view size
# renderView1_1.ViewSize = [1923, 1610]

# get layout
layout1_1 = GetLayout()

# show data in view
spatial_data_001dDisplay = Show(spatial_data_001d, renderView1_1, 'UnstructuredGridRepresentation')

# get color transfer function/color map for 'WindSpeed'
windSpeedLUT = GetColorTransferFunction('WindSpeed')
windSpeedLUT.RGBPoints = [0.0, 0.231373, 0.298039, 0.752941, 19.997047424316406, 0.865003, 0.865003, 0.865003, 39.99409484863281, 0.705882, 0.0156863, 0.14902]
windSpeedLUT.ScalarRangeInitialized = 1.0

# get opacity transfer function/opacity map for 'WindSpeed'
windSpeedPWF = GetOpacityTransferFunction('WindSpeed')
windSpeedPWF.Points = [0.0, 0.0, 0.5, 0.0, 39.99409484863281, 1.0, 0.5, 0.0]
windSpeedPWF.ScalarRangeInitialized = 1

# trace defaults for the display properties.
spatial_data_001dDisplay.Representation = 'Surface'
spatial_data_001dDisplay.ColorArrayName = ['POINTS', 'WindSpeed']
spatial_data_001dDisplay.LookupTable = windSpeedLUT
spatial_data_001dDisplay.OSPRayScaleArray = 'WindSpeed'
spatial_data_001dDisplay.OSPRayScaleFunction = 'PiecewiseFunction'
spatial_data_001dDisplay.SelectOrientationVectors = 'WindVelocity'
spatial_data_001dDisplay.ScaleFactor = 148160.0
spatial_data_001dDisplay.SelectScaleArray = 'WindSpeed'
spatial_data_001dDisplay.GlyphType = 'Arrow'
spatial_data_001dDisplay.GlyphTableIndexArray = 'WindSpeed'
spatial_data_001dDisplay.GaussianRadius = 7408.0
spatial_data_001dDisplay.SetScaleArray = ['POINTS', 'WindSpeed']
spatial_data_001dDisplay.ScaleTransferFunction = 'PiecewiseFunction'
spatial_data_001dDisplay.OpacityArray = ['POINTS', 'WindSpeed']
spatial_data_001dDisplay.OpacityTransferFunction = 'PiecewiseFunction'
spatial_data_001dDisplay.DataAxesGrid = 'GridAxesRepresentation'
spatial_data_001dDisplay.PolarAxes = 'PolarAxesRepresentation'
spatial_data_001dDisplay.ScalarOpacityFunction = windSpeedPWF
spatial_data_001dDisplay.ScalarOpacityUnitDistance = 54300.1260708117

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
spatial_data_001dDisplay.ScaleTransferFunction.Points = [0.0, 0.0, 0.5, 0.0, 39.99409484863281, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
spatial_data_001dDisplay.OpacityTransferFunction.Points = [0.0, 0.0, 0.5, 0.0, 39.99409484863281, 1.0, 0.5, 0.0]

# get the material library
materialLibrary1_1 = GetMaterialLibrary()

# show color bar/color legend
#spatial_data_001dDisplay.SetScalarBarVisibility(renderView1_1, True)

# update the view to ensure updated data information
renderView1_1.Update()

#change interaction mode for render view
renderView1_1.InteractionMode = '3D'

# reset view to fit data bounds
renderView1_1.ResetCamera()

# current camera placement for renderView1_1
renderView1_1.CameraPosition = [-119156.02677993872, 276523.4214322601, 4036591.8681269446]
renderView1_1.CameraViewUp = [0.0, 1.0, 0.0]
renderView1_1.CameraParallelScale = 1047649.4070059888

# change representation type
spatial_data_001dDisplay.SetRepresentationType('Outline')

# create a new 'Point Source'
pointSource1_1 = PointSource()

# Properties modified on pointSource1_1
pointSource1_1.NumberOfPoints = 400
pointSource1_1.Radius = 1400000.0

# show data in view
pointSource1_1Display = Show(pointSource1_1, renderView1_1, 'GeometryRepresentation')

# trace defaults for the display properties.
pointSource1_1Display.Representation = 'Surface'
pointSource1_1Display.ColorArrayName = [None, '']
pointSource1_1Display.OSPRayScaleFunction = 'PiecewiseFunction'
pointSource1_1Display.SelectOrientationVectors = 'None'
pointSource1_1Display.ScaleFactor = 275352.7125
pointSource1_1Display.SelectScaleArray = 'None'
pointSource1_1Display.GlyphType = 'Arrow'
pointSource1_1Display.GlyphTableIndexArray = 'None'
pointSource1_1Display.GaussianRadius = 13767.635625
pointSource1_1Display.SetScaleArray = [None, '']
pointSource1_1Display.ScaleTransferFunction = 'PiecewiseFunction'
pointSource1_1Display.OpacityArray = [None, '']
pointSource1_1Display.OpacityTransferFunction = 'PiecewiseFunction'
pointSource1_1Display.DataAxesGrid = 'GridAxesRepresentation'
pointSource1_1Display.PolarAxes = 'PolarAxesRepresentation'

# update the view to ensure updated data information
renderView1_1.Update()

# create a new 'Transform'
transform1_1 = Transform(Input=pointSource1_1)
transform1_1.Transform = 'Transform'

# Properties modified on transform1_1.Transform
transform1_1.Transform.Scale = [1.0, 1.0, 0.0]

# show data in view
transform1_1Display = Show(transform1_1, renderView1_1, 'GeometryRepresentation')

# trace defaults for the display properties.
transform1_1Display.Representation = 'Surface'
transform1_1Display.ColorArrayName = [None, '']
transform1_1Display.OSPRayScaleFunction = 'PiecewiseFunction'
transform1_1Display.SelectOrientationVectors = 'None'
transform1_1Display.ScaleFactor = 275352.7125
transform1_1Display.SelectScaleArray = 'None'
transform1_1Display.GlyphType = 'Arrow'
transform1_1Display.GlyphTableIndexArray = 'None'
transform1_1Display.GaussianRadius = 13767.635625
transform1_1Display.SetScaleArray = [None, '']
transform1_1Display.ScaleTransferFunction = 'PiecewiseFunction'
transform1_1Display.OpacityArray = [None, '']
transform1_1Display.OpacityTransferFunction = 'PiecewiseFunction'
transform1_1Display.DataAxesGrid = 'GridAxesRepresentation'
transform1_1Display.PolarAxes = 'PolarAxesRepresentation'

# hide data in view
Hide(pointSource1_1, renderView1_1)

# update the view to ensure updated data information
renderView1_1.Update()

# set active source
SetActiveSource(spatial_data_001d)

# toggle 3D widget visibility (only when running from the GUI)
Hide3DWidgets(proxy=transform1_1.Transform)

# create a new 'Stream Tracer With Custom Source'
streamTracerWithCustomSource1_1 = StreamTracerWithCustomSource(Input=spatial_data_001d,
    SeedSource=transform1_1)
streamTracerWithCustomSource1_1.Vectors = ['POINTS', 'WindVelocity']
streamTracerWithCustomSource1_1.MaximumStreamlineLength = 1481600.0

# Properties modified on streamTracerWithCustomSource1_1
streamTracerWithCustomSource1_1.InitialStepLength = 1.0

# show data in view
streamTracerWithCustomSource1_1Display = Show(streamTracerWithCustomSource1_1, renderView1_1, 'GeometryRepresentation')

# trace defaults for the display properties.
streamTracerWithCustomSource1_1Display.Representation = 'Surface'
streamTracerWithCustomSource1_1Display.ColorArrayName = ['POINTS', 'WindSpeed']
streamTracerWithCustomSource1_1Display.LookupTable = windSpeedLUT
streamTracerWithCustomSource1_1Display.OSPRayScaleArray = 'WindSpeed'
streamTracerWithCustomSource1_1Display.OSPRayScaleFunction = 'PiecewiseFunction'
streamTracerWithCustomSource1_1Display.SelectOrientationVectors = 'Normals'
streamTracerWithCustomSource1_1Display.ScaleFactor = 148118.5625
streamTracerWithCustomSource1_1Display.SelectScaleArray = 'WindSpeed'
streamTracerWithCustomSource1_1Display.GlyphType = 'Arrow'
streamTracerWithCustomSource1_1Display.GlyphTableIndexArray = 'WindSpeed'
streamTracerWithCustomSource1_1Display.GaussianRadius = 7405.928125
streamTracerWithCustomSource1_1Display.SetScaleArray = ['POINTS', 'WindSpeed']
streamTracerWithCustomSource1_1Display.ScaleTransferFunction = 'PiecewiseFunction'
streamTracerWithCustomSource1_1Display.OpacityArray = ['POINTS', 'WindSpeed']
streamTracerWithCustomSource1_1Display.OpacityTransferFunction = 'PiecewiseFunction'
streamTracerWithCustomSource1_1Display.DataAxesGrid = 'GridAxesRepresentation'
streamTracerWithCustomSource1_1Display.PolarAxes = 'PolarAxesRepresentation'

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
streamTracerWithCustomSource1_1Display.ScaleTransferFunction.Points = [3.21201156339157e-07, 0.0, 0.5, 0.0, 39.98784637451172, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
streamTracerWithCustomSource1_1Display.OpacityTransferFunction.Points = [3.21201156339157e-07, 0.0, 0.5, 0.0, 39.98784637451172, 1.0, 0.5, 0.0]

# hide data in view
Hide(transform1_1, renderView1_1)

# update the view to ensure updated data information
renderView1_1.Update()

# create a new 'Tube'
tube1_1 = Tube(Input=streamTracerWithCustomSource1_1)
tube1_1.Scalars = ['POINTS', 'WindSpeed']
tube1_1.Vectors = ['POINTS', 'Normals']
tube1_1.Radius = 12000
tube1_1.NumberofSides = 36  # default number of sides is 6 (hexagonal tube)

# show data in view
tube1_1Display = Show(tube1_1, renderView1_1, 'GeometryRepresentation')

# trace defaults for the display properties.
tube1_1Display.Representation = 'Surface'
tube1_1Display.ColorArrayName = ['POINTS', 'WindSpeed']
tube1_1Display.LookupTable = windSpeedLUT
tube1_1Display.OSPRayScaleArray = 'WindSpeed'
tube1_1Display.OSPRayScaleFunction = 'PiecewiseFunction'
tube1_1Display.SelectOrientationVectors = 'Normals'
tube1_1Display.ScaleFactor = 150760.98750000002
tube1_1Display.SelectScaleArray = 'WindSpeed'
tube1_1Display.GlyphType = 'Arrow'
tube1_1Display.GlyphTableIndexArray = 'WindSpeed'
tube1_1Display.GaussianRadius = 7538.0493750000005
tube1_1Display.SetScaleArray = ['POINTS', 'WindSpeed']
tube1_1Display.ScaleTransferFunction = 'PiecewiseFunction'
tube1_1Display.OpacityArray = ['POINTS', 'WindSpeed']
tube1_1Display.OpacityTransferFunction = 'PiecewiseFunction'
tube1_1Display.DataAxesGrid = 'GridAxesRepresentation'
tube1_1Display.PolarAxes = 'PolarAxesRepresentation'


# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
tube1_1Display.ScaleTransferFunction.Points = [3.21201156339157e-07, 0.0, 0.5, 0.0, 39.98784637451172, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
tube1_1Display.OpacityTransferFunction.Points = [3.21201156339157e-07, 0.0, 0.5, 0.0, 39.98784637451172, 1.0, 0.5, 0.0]

# hide data in view
Hide(streamTracerWithCustomSource1_1, renderView1_1)

# update the view to ensure updated data information
renderView1_1.Update()

# create a new 'Warp By Scalar'
warpByScalar1_1 = WarpByScalar(Input=tube1_1)
warpByScalar1_1.Scalars = ['POINTS', 'WindSpeed']

# Properties modified on warpByScalar1_1
warpByScalar1_1.ScaleFactor = 10000.0
warpByScalar1_1.UseNormal = 1

# show data in view
warpByScalar1_1Display = Show(warpByScalar1_1, renderView1_1, 'GeometryRepresentation')

# trace defaults for the display properties.
warpByScalar1_1Display.Representation = 'Surface'
warpByScalar1_1Display.ColorArrayName = ['POINTS', 'WindSpeed']
warpByScalar1_1Display.LookupTable = windSpeedLUT
warpByScalar1_1Display.OSPRayScaleArray = 'WindSpeed'
warpByScalar1_1Display.OSPRayScaleFunction = 'PiecewiseFunction'
warpByScalar1_1Display.SelectOrientationVectors = 'Normals'
warpByScalar1_1Display.ScaleFactor = 150760.98750000002
warpByScalar1_1Display.SelectScaleArray = 'WindSpeed'
warpByScalar1_1Display.GlyphType = 'Arrow'
warpByScalar1_1Display.GlyphTableIndexArray = 'WindSpeed'
warpByScalar1_1Display.GaussianRadius = 7538.0493750000005
warpByScalar1_1Display.SetScaleArray = ['POINTS', 'WindSpeed']
warpByScalar1_1Display.ScaleTransferFunction = 'PiecewiseFunction'
warpByScalar1_1Display.OpacityArray = ['POINTS', 'WindSpeed']
warpByScalar1_1Display.OpacityTransferFunction = 'PiecewiseFunction'
warpByScalar1_1Display.DataAxesGrid = 'GridAxesRepresentation'
warpByScalar1_1Display.PolarAxes = 'PolarAxesRepresentation'

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
warpByScalar1_1Display.ScaleTransferFunction.Points = [3.21201156339157e-07, 0.0, 0.5, 0.0, 39.98784637451172, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
warpByScalar1_1Display.OpacityTransferFunction.Points = [3.21201156339157e-07, 0.0, 0.5, 0.0, 39.98784637451172, 1.0, 0.5, 0.0]

# hide data in view
Hide(tube1_1, renderView1_1)

# update the view to ensure updated data information
renderView1_1.Update()

camera = GetActiveCamera()
camera.SetViewUp(0,1,0)
camera.Pitch(50)
# reset view to fit data bounds
renderView1_1.ResetCamera()

camera.Dolly(1.5)

# update the view to ensure updated data information
renderView1_1.Update()

# Hide orientation axes
renderView1_1.OrientationAxesVisibility = 0

# save screenshot
SaveScreenshot('/home/jason/projects/NCFS/WindTest/Irene/hindcast/temp/stuff.png', renderView1_1, ImageResolution=[1920, 1610])
