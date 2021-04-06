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

# Create a structured grid with these points
#meshReader = vtk.vtkUnstructuredGridReader()
#meshReader = vtk.vtkXMLUnstructuredGridReader()
#meshReader = vtk.vtkPVDReader()
meshReader = paraview.simple.PVDReader(FileName="/home/jason/adcirc-cg/examples/shinnecock_inlet/hwind/vtu/fort.14_fort.74.pvd")
# set file name
#input_file = 'spatial_data_%03d.d' % int(options.frame)
#input_file = 'v6brivers.14_fort.74_%03d.vtu' % int(frame)
#input_file = 'v6brivers.14_fort.74.pvd'
#input_file = 'femar3_irene30_maxwvel.vtu'
#meshReader.SetScalarsName("WindSpeed")
#meshReader.SetVectorsName("WindVelocity")
#meshReader.SetPointArrayStatus("WindVelocity",1)
##meshReader.SetPointArrayStatus("MaximumWindSpeed",1)
#meshReader.Update()