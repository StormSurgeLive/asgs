#!/usr/bin/env python

import vtk
import Numeric as N
import sys
from vtk.util.colors import *

frame = 1
interact = 0
# get command line argument for the frame number
if (len(sys.argv) > 1):
    frame = sys.argv[1]
if (len(sys.argv) > 2):
    if (sys.argv[2] == "-i"):
        interact = 1

# Create a structured grid with these points
sgridReader = vtk.vtkStructuredGridReader()
input_file = 'spatial_data_%03d.d' % int(frame)
sgridReader.SetFileName(input_file)
sgridReader.SetScalarsName("WindSpeed")
sgridReader.Update()

# create a color scale (color lookup table)
lut = vtk.vtkLookupTable()
lut.SetNumberOfColors(256)
#lut.SetHueRange(0.0, 0.667)
lut.Build()
plane = vtk.vtkStructuredGridGeometryFilter()
plane.SetInputConnection(sgridReader.GetOutputPort())
planeMapper = vtk.vtkPolyDataMapper()
planeMapper.SetInputConnection(plane.GetOutputPort())
planeMapper.SetScalarRange(sgridReader.GetOutput().GetScalarRange())
planeMapper.SetLookupTable(lut)
gridActor = vtk.vtkActor()
gridActor.SetMapper(planeMapper)
#gridActor.GetProperty().SetRepresentationToWireframe()

outline = vtk.vtkStructuredGridOutlineFilter()
outline.SetInputConnection(sgridReader.GetOutputPort())
outlineMapper = vtk.vtkPolyDataMapper()
outlineMapper.SetInputConnection(outline.GetOutputPort())
outlineActor = vtk.vtkActor()
outlineActor.SetMapper(outlineMapper)
outlineActor.GetProperty().SetColor(1,1,1)

warp = vtk.vtkWarpScalar()
warp.SetInputConnection(plane.GetOutputPort())
#warp.XYPlaneOn()
warp.SetNormal(0.0,0.0,1.0)
warp.UseNormalOn()
warp.SetScaleFactor(10.0)

normals = vtk.vtkPolyDataNormals()
normals.SetInputConnection(warp.GetOutputPort())
normals.SetFeatureAngle(60.0)
warpMapper = vtk.vtkPolyDataMapper()
warpMapper.SetLookupTable(lut)
warpMapper.SetInputConnection(normals.GetOutputPort())
warpMapper.SetScalarRange(sgridReader.GetOutput().GetScalarRange())
warpActor = vtk.vtkActor()
warpActor.SetMapper(warpMapper)

# create streamlines
seedsSphere = vtk.vtkPointSource()
seedsSphere.SetRadius(740.8)
seedsSphere.SetCenter(0.0, 0.0, 0.0)
seedsSphere.SetNumberOfPoints(2400)
seedTransform = vtk.vtkTransform()
seedTransform.Scale(1.0,1.0,0.0)
seedTransform.RotateZ(1.0*float(frame)) # 1 degree
seedFilter = vtk.vtkTransformPolyDataFilter()
seedFilter.SetTransform(seedTransform)
seedFilter.SetInputConnection(seedsSphere.GetOutputPort())


integ = vtk.vtkRungeKutta4()
streamer = vtk.vtkStreamTracer()
streamer.SetInputConnection(sgridReader.GetOutputPort())
#streamer.SetStartPosition(-0.77236544E+02, 0.28891293E+02, 0.00000000E+00)
streamer.SetSource(seedFilter.GetOutput())
streamer.SetMaximumPropagation(100)
#streamer.SetMaximumPropagationUnitToTimeUnit()
streamer.SetInitialIntegrationStep(1.0)
#streamer.SetInitialIntegrationStepUnitToCellLengthUnit()
#streamer.SetIntegrationDirectionToBoth()
streamer.SetIntegrator(integ)
streamTube = vtk.vtkTubeFilter()
streamTube.SetInputConnection(streamer.GetOutputPort())
#streamTube.SetInputArrayToProcess(1,0,0,vtkDataObject::FIELD_ASSOCIATION_POINTS, vectors)
streamTube.SetRadius(10.0)
streamTube.SetNumberOfSides(12)
streamTube.SetVaryRadiusToVaryRadiusByVector()
streamWarp = vtk.vtkWarpScalar()
streamWarp.SetInputConnection(streamTube.GetOutputPort())
streamWarp.SetNormal(0.0,0.0,1.0)
streamWarp.UseNormalOn()
streamWarp.SetScaleFactor(10.0)
mapStreamTube = vtk.vtkPolyDataMapper()
mapStreamTube.SetInputConnection(streamWarp.GetOutputPort())
mapStreamTube.SetScalarRange(sgridReader.GetOutput().GetPointData().GetScalars().GetRange())
mapStreamTube.SetLookupTable(lut)
streamTubeActor = vtk.vtkActor()
streamTubeActor.SetMapper(mapStreamTube)
#streamTubeActor.GetProperty().BackfaceCullingOn()

# This creates a blue to red lut.
lut.SetHueRange(0.667, 0.0)

# Create the rendering window, renderer, and interactive renderer
ren = vtk.vtkRenderer()
cam = ren.GetActiveCamera()
#cam.Elevation(-30)
cam.Pitch(30)
renWin = vtk.vtkRenderWindow()
renWin.AddRenderer(ren)
iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renWin)

# Add the actors to the renderer, set the background and size
#ren.AddActor(gridActor)
ren.AddActor(outlineActor)
ren.AddActor(streamTubeActor)
#ren.AddActor(warpActor)
ren.SetBackground(1, 1, 1)
renWin.SetSize(650, 650)
#if ( interact == 0 ):
#    renWin.OffScreenRenderingOn()

#cam.Dolly(10)
ren.ResetCamera()
cam.Zoom(1.5)
renWin.Render()

# write a png
w2if = vtk.vtkWindowToImageFilter()
w2if.SetInput(renWin)
w2if.Update()
 
writer = vtk.vtkPNGWriter()
filename = 'spatial_data_%03d.png' % int(frame)
writer.SetFileName(filename)
writer.SetInput(w2if.GetOutput())
writer.Write()

#px = vtk.vtkPOVExporter()
#px.SetFileName("stuff.pov")
#px.SetRenderWindow(renWin)
#px.Write()

# Interact with the data.
if (interact == 1):
    iren.Initialize()
    iren.Start()
