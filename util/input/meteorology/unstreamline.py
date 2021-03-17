#!/usr/bin/env python

import vtk
import sys
from optparse import OptionParser
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
meshReader = vtk.vtkUnstructuredGridReader()
#meshReader = vtk.vtkXMLUnstructuredGridReader()
#meshReader = vtk.vtkPVDReader()
input_file = 'spatial_data_%03d.d' % int(frame)
#input_file = 'v6brivers.14_fort.74_%03d.vtu' % int(frame)
#input_file = 'v6brivers.14_fort.74.pvd'
#input_file = 'femar3_irene30_maxwvel.vtu'
meshReader.SetFileName(input_file)
meshReader.SetScalarsName("WindSpeed")
meshReader.SetVectorsName("WindVelocity")
#meshReader.SetPointArrayStatus("WindVelocity",1)
##meshReader.SetPointArrayStatus("MaximumWindSpeed",1)
meshReader.Update()

# create actor for unstructured grid outline
#outlineMesh = vtk.vtkOutlineFilter()
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
gridActor.SetScale(1.0,1.0,0.0000001)
gridActor.GetProperty().SetRepresentationToWireframe()


# create streamlines
#xmin= -1953764.5423199304  xmax= 1473590.7094357659
#ymin= -3071352.8797937827  ymax= 1150611.477018431
seedsSphere = vtk.vtkPointSource()
seedsSphere.SetRadius(3071352.0)
seedsSphere.SetCenter(0.0, 0.0, 0.0)
seedsSphere.SetNumberOfPoints(5000)
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
streamer.SetIntegrationDirectionToBoth()
streamer.SetIntegrator(integ)
#
streamTube = vtk.vtkTubeFilter()
streamTube.SetInputConnection(streamer.GetOutputPort())
#streamTube.SetInputArrayToProcess(1,0,0,vtkDataObject::FIELD_ASSOCIATION_POINTS, vectors)
streamTube.SetRadius(5000.0)
streamTube.SetNumberOfSides(12)

streamWarp = vtk.vtkWarpScalar()
streamWarp.SetInputConnection(streamTube.GetOutputPort())
streamWarp.SetNormal(0.0,0.0,1.0)
streamWarp.UseNormalOn()
streamWarp.SetScaleFactor(10000.0)

mapStreamTube = vtk.vtkPolyDataMapper()
mapStreamTube.SetInputConnection(streamWarp.GetOutputPort())
#mapStreamTube.SetInputConnection(streamer.GetOutputPort())
#mapStreamTube.SetInputConnection(streamTube.GetOutputPort())
mapStreamTube.SetScalarRange(meshReader.GetOutput().GetPointData().GetScalars().GetRange())

mapStreamTube.SetLookupTable(lut)
streamTubeActor = vtk.vtkActor()
streamTubeActor.SetMapper(mapStreamTube)
streamTubeActor.GetProperty().SetColor(0.0,0.0,0.0)

# Create the usual rendering stuff.
ren = vtk.vtkRenderer()
renWin = vtk.vtkRenderWindow()
renWin.AddRenderer(ren)
renWin.SetSize(700, 700)
iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renWin)

ren.SetBackground(1.0, 1.0, 1.0)
ren.AddActor(gridActor)
ren.AddActor(outlineActor)
ren.AddActor(streamTubeActor)
ren.ResetCamera()
cam = ren.GetActiveCamera()
cam.Zoom(1.3)
if ( interact == 0 ):
    renWin.OffScreenRenderingOn()
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

# Interact with the data.
if (interact == 1):
    iren.Initialize()
    iren.Start()
