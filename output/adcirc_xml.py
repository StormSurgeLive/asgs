#!/usr/bin/env python

import vtk
import sys
from optparse import OptionParser
from vtk.util.colors import *
#
# parse command line options
parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", default="fort.14",
                  help="file to read")
parser.add_option("-i", "--interact", dest="interact", default=False,
                  action="store_true", help="to enable interaction with data")
parser.add_option("-a", "--variable", dest="variable", 
                  default="BathymetricDepth", help="variable to visualize")
                  
(options, args) = parser.parse_args()

# set up offscreen rendering
#graphics_factory = vtk.vtkGraphicsFactory()
#graphics_factory.SetOffScreenOnlyMode(1)
#graphics_factory.SetUseMesaClasses(1)

#imaging_factory = vtk.vtkImagingFactory()
#imaging_factory.SetUseMesaClasses(1)

#cdp = vtk.vtkCompositeDataPipeline()
#vtkAlgorithm::SetDefaultExecutivePrototype(cdp)

# Create an unstructured grid reader
meshReader = vtk.vtkXMLUnstructuredGridReader()
meshReader.SetFileName(options.filename)
print 'Reading ',options.variable,' from ',options.filename,'.'
meshReader.SetPointArrayStatus(options.variable,1)
if options.variable != 'BathymetricDepth':
    meshReader.SetPointArrayStatus('BathymetricDepth',0)
meshReader.Update()

# create actor for unstructured grid outline
meshGeometryFilter = vtk.vtkGeometryFilter()
meshGeometryFilter.SetInput(meshReader.GetOutput())
outlineMesh = vtk.vtkFeatureEdges()
outlineMesh.SetInputConnection(meshGeometryFilter.GetOutputPort())
outlineMesh.BoundaryEdgesOn()
outlineMeshMapper = vtk.vtkPolyDataMapper()
outlineMeshMapper.SetInputConnection(outlineMesh.GetOutputPort())
outlineActor = vtk.vtkActor()
outlineActor.SetMapper(outlineMeshMapper)
outlineActor.GetProperty().SetColor(1,1,1)

# create a color scale (color lookup table)
refLut = vtk.vtkLookupTable()
lut = vtk.vtkLookupTable()
refLut.SetNumberOfColors(256)
lut.SetNumberOfColors(256)
refLut.SetHueRange(0.0, 0.667)
refLut.Build()
lut.Build()
for j in range(256):
    lut.SetTableValue(j, refLut.GetTableValue(255-j))

#
planeMapper = vtk.vtkDataSetMapper()
planeMapper.SetInputConnection(meshReader.GetOutputPort())
planeMapper.SetScalarRange(meshReader.GetOutput().GetScalarRange())
planeMapper.SetLookupTable(lut)
gridActor = vtk.vtkActor()
gridActor.SetMapper(planeMapper)
#gridActor.GetProperty().SetRepresentationToWireframe()

# create streamlines
if options.variable == 'WindVelocity':
    print 'Generating streamlines.'
    seedsSphere = vtk.vtkPointSource()
    seedsSphere.SetRadius(3000000.0)
    seedsSphere.SetCenter(-33000000.0, 3000000.0, 0.0)
    seedsSphere.SetNumberOfPoints(10000)
    seedTransform = vtk.vtkTransform()
    seedTransform.Scale(1.0,1.0,0.0)
    #seedTransform.RotateZ(1.0*float(frame)) # 1 degree
    seedFilter = vtk.vtkTransformPolyDataFilter()
    seedFilter.SetTransform(seedTransform)
    seedFilter.SetInputConnection(seedsSphere.GetOutputPort())

    integ = vtk.vtkRungeKutta4()
    streamer = vtk.vtkStreamTracer()
    streamer.SetInputConnection(meshReader.GetOutputPort())
    #streamer.SetStartPosition(0.18474886E+01, 0.12918899E+00, 0.00000000E+00)
    streamer.SetSource(seedFilter.GetOutput())
    streamer.SetMaximumPropagation(160000.0)
    #streamer.SetMaximumPropagationUnitToTimeUnit()
    streamer.SetInitialIntegrationStep(1.0)
    #streamer.SetInitialIntegrationStepUnitToCellLengthUnit()
    #streamer.SetIntegrationDirectionToBoth()
    streamer.SetIntegrator(integ)
    #
    streamTube = vtk.vtkTubeFilter()
    streamTube.SetInputConnection(streamer.GetOutputPort())
    #streamTube.SetInputArrayToProcess(1,0,0,vtkDataObject::FIELD_ASSOCIATION_POINTS, vectors)
    streamTube.SetRadius(5000.0)
    streamTube.SetNumberOfSides(12)
    #streamTube.SetVaryRadiusToVaryRadiusByVector()

    mapStreamTube = vtk.vtkPolyDataMapper()
    mapStreamTube.SetInputConnection(streamTube.GetOutputPort())
    mapStreamTube.SetScalarRange(meshReader.GetOutput().GetPointData().GetScalars().GetRange())
    mapStreamTube.SetLookupTable(lut)
    streamTubeActor = vtk.vtkActor()
    streamTubeActor.SetMapper(mapStreamTube)
    streamTubeActor.GetProperty().SetColor(0.0,0.0,0.0)
    ##streamTubeActor.GetProperty().BackfaceCullingOn()

# Create the usual rendering stuff.
ren = vtk.vtkRenderer()
renWin = vtk.vtkRenderWindow()
renWin.AddRenderer(ren)
renWin.SetSize(700, 700)
iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renWin)

ren.SetBackground(1.0, 1.0, 1.0)
ren.AddActor(outlineActor)
if options.variable != 'WindVelocity':
    ren.AddActor(gridActor)
else:
    ren.AddActor(streamTubeActor)

ren.ResetCamera()
cam = ren.GetActiveCamera()
cam.Zoom(1.5)
if options.interact == False:
    renWin.OffScreenRenderingOn()
renWin.Render()

# write a png
#w2if = vtk.vtkWindowToImageFilter()
#w2if.SetInput(renWin)
#w2if.Update()
 
#writer = vtk.vtkPNGWriter()
#filename = 'spatial_data_%03d.png' % int(frame)
#writer.SetFileName(filename)
#writer.SetInput(w2if.GetOutput())
#writer.Write()

# Interact with the data.
if options.interact == True:
    iren.Initialize()
    iren.Start()






#integ = vtk.vtkRungeKutta4()
#streamer = vtk.vtkStreamTracer()
##streamer.SetInputConnection(meshReader.GetOutputPort())
##streamer.SetStartPosition(-0.77236544E+02, 0.28891293E+02, 0.00000000E+00)
#streamer.SetSource(seedFilter.GetOutput())
#streamer.SetMaximumPropagation(100)
##streamer.SetMaximumPropagationUnitToTimeUnit()
#streamer.SetInitialIntegrationStep(1.0)
#streamer.SetInitialIntegrationStepUnitToCellLengthUnit()
#streamer.SetIntegrationDirectionToBoth()
#streamer.SetIntegrator(integ)
#streamTube = vtk.vtkTubeFilter()
#streamTube.SetInputConnection(streamer.GetOutputPort())
#streamTube.SetInputArrayToProcess(1,0,0,vtkDataObject::FIELD_ASSOCIATION_POINTS, vectors)
#streamTube.SetRadius(10.0)
#streamTube.SetNumberOfSides(12)
#streamTube.SetVaryRadiusToVaryRadiusByVector()
#streamWarp = vtk.vtkWarpScalar()
#streamWarp.SetInputConnection(streamTube.GetOutputPort())
#streamWarp.SetNormal(0.0,0.0,1.0)
#streamWarp.UseNormalOn()
#streamWarp.SetScaleFactor(10.0)
#mapStreamTube = vtk.vtkPolyDataMapper()
#mapStreamTube.SetInputConnection(streamWarp.GetOutputPort())
#mapStreamTube.SetScalarRange(meshReader.GetOutput().GetPointData().GetScalars().GetRange())
#mapStreamTube.SetLookupTable(lut)
#streamTubeActor = vtk.vtkActor()
#streamTubeActor.SetMapper(mapStreamTube)
##streamTubeActor.GetProperty().BackfaceCullingOn()

# This creates a blue to red lut.
#lut.SetHueRange(0.667, 0.0)

# Create the rendering window, renderer, and interactive renderer
#ren = vtk.vtkRenderer()
#cam = ren.GetActiveCamera()
##cam.Elevation(-30)
#cam.Pitch(30)
#renWin = vtk.vtkRenderWindow()
#renWin.AddRenderer(ren)
#iren = vtk.vtkRenderWindowInteractor()
#iren.SetRenderWindow(renWin)

# Add the actors to the renderer, set the background and size
#ren.AddActor(gridActor)
#ren.AddActor(outlineActor)
#ren.AddActor(streamTubeActor)
#ren.AddActor(warpActor)
#ren.SetBackground(0, 0, 0)
#renWin.SetSize(650, 650)
#if ( interact == 0 ):
#    renWin.OffScreenRenderingOn()

#cam.Dolly(10)
#ren.ResetCamera()
#cam.Zoom(1.5)
#renWin.Render()

# write a png
#w2if = vtk.vtkWindowToImageFilter()
#w2if.SetInput(renWin)
#w2if.Update()
 
#writer = vtk.vtkPNGWriter()
#filename = 'spatial_data_%03d.png' % int(frame)
#writer.SetFileName(filename)
#writer.SetInput(w2if.GetOutput())
#writer.Write()

#px = vtk.vtkPOVExporter()
#px.SetFileName("stuff.pov")
#px.SetRenderWindow(renWin)
#px.Write()

# Interact with the data.
#if (interact == 1):
#    iren.Initialize()
#    iren.Start()
