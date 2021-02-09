"""
A snippet for add cell center coordinates to the CellData of an input dataset.

Created by Bane Sullivan <banesullivan@gmail.com> for the PVGeo project <http://pvgeo.org>
"""
from paraview.util.vtkAlgorithm import *
from vtk.util.vtkAlgorithm import VTKPythonAlgorithmBase
import numpy as np
import vtk
from vtk.util import numpy_support as nps
from vtk.numpy_interface import dataset_adapter as dsa

MENU_CAT = 'Custom Filters'

# Extract Cell Centers
@smproxy.filter(name='AppendCellCenters', label='Append Cell Centers')
@smhint.xml('''<ShowInMenu category="%s"/>
    <RepresentationType view="RenderView" type="Surface" />''' % MENU_CAT)
@smproperty.input(name="Input", port_index=0)
@smdomain.datatype(dataTypes=["vtkDataSet"], composite_data_supported=True)
class AppendCellCenters(VTKPythonAlgorithmBase):
    """A filter to extract the cell centers of a data object and append those
    coordinates as a 3-component double array on the Cell Data.

    Created by Bane Sullivan <banesullivan@gmail.com> for the PVGeo project <http://pvgeo.org>
    """
    def __init__(self, **kwargs):
        VTKPythonAlgorithmBase.__init__(self, nInputPorts=1, nOutputPorts=1, inputType='vtkDataSet')

    # THIS IS CRUCIAL to preserve data type through filter
    def RequestDataObject(self, request, inInfo, outInfo):
        """This method lets the pipeline know that the algorithm will dynamically decide
        the output data type based in the input data type.
        """
        self.OutputType = self.GetInputData(inInfo, 0, 0).GetClassName()
        self.FillOutputPortInformation(0, outInfo.GetInformationObject(0))
        return 1

    def RequestData(self, request, inInfoVec, outInfoVec):
        pdi = self.GetInputData(inInfoVec, 0, 0)
        pdo = self.GetOutputData(outInfoVec, 0)
        # Find cell centers
        filt = vtk.vtkCellCenters()
        filt.SetInputDataObject(pdi)
        filt.Update()
        # I use the dataset adapter/numpy interface because its easy
        centers = dsa.WrapDataObject(filt.GetOutput()).Points
        centers = nps.numpy_to_vtk(centers)
        centers.SetName('Cell Centers')
        # Copy input data and add cell centers as tuple array
        pdo.DeepCopy(pdi)
        pdo.GetCellData().AddArray(centers)
        return 1